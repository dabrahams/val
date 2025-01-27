import Compiler
import DequeModule
import Foundation
import Utils

/// A virtual machine that interprets VIL code.
///
/// Asynchronous values
/// -------------------
/// The interpreter relies on cooperative multitasking to evaluate async values. An `async`
/// instruction creates a new coroutine, represented as a thick function without any argument,
/// that's added to a set. When the active coroutine (initially `main`) evaluates an `await`
/// instruction, it is suspended and the interpreter resumes another coroutine. The next active
/// coroutine is selected at random, in an effort to invalidate programs that would (perhaps
/// unintendedly) rely on a deterministic execution.
public struct Interpreter {

  /// The address of a function.

  /// A key identifying a value witness table.
  struct ValueWitnessTableKey: Equatable {

    /// The underlying index in the array of witness tables, offset by one.
    fileprivate var rawValue: Int

    fileprivate var index: Int { rawValue - 1 }

    static var zero: ValueWitnessTableKey { ValueWitnessTableKey(rawValue: 0) }

  }

  /// The initial capacity of new register stacks.
  let registerStackCapacity: Int

  /// The initial capacity of new call stacks.
  let callStackCapacity: Int

  /// The standard output of the interpreter.
  let standardOutput: FileHandle

  /// The status of the interpreter.
  var status: Int = 0

  /// The data layout used to calculate type layout information.
  private var layout = DataLayout()

  /// The modules loaded into the interpreter.
  private var modules: [String: Module] = [:]

  /// A table mapping a function name to its virtual address.
  private var funTable: [String: (moduleID: String, funName: String)] = [:]

  /// The view witness tables loaded into the interpreter.
  private var viewWitnessTables: [ViewWitnessTable] = []

  /// The value witness tables loaded into the interpreter.
  private var valueWitnessTables: [ValueWitnessTable] = []

  /// A table mapping VIL types to their value witness table.
  private var typeToValueWitnessTableKey: ReferenceTable<ValType, ValueWitnessTableKey> = [:]

  /// The virtual threads managed by the interpreter.
  private var threads: [VirtualThread.ID: VirtualThread] = [:]

  /// The thread dependency graph.
  ///
  /// This graph denotes the dependencies between all running threads as a relation `D` such that
  /// `(t, u) \in D` means `t` is waiting for `u` to terminate. New elements of that relation are
  /// created whenever a thread evaluates an `await` expression on a still running asynchronous
  /// expression, and are removed whenever a thread terminates.
  private var threadDependencyGraph = ThreadDependencyGraph()

  /// The ID of the active thread.
  private var activeThreadID: VirtualThread.ID = 0

  /// A factory to create thread identifiers.
  private var threadIDFactory = AutoIncrementFactory(start: 1)

  /// Initializes a new interpreter.
  ///
  /// - Parameters:
  ///   - regiterStackCapacity: The initial capacity of the register stack, in bytes.
  ///   - callStackCapacity: The initial capacity of the call stack, in bytes.
  public init(
    registerStackCapacity: Int = 1_048_576,
    callStackCapacity: Int = 1_048_576,
    standardOutput: FileHandle = .standardOutput
  ) {
    self.registerStackCapacity = registerStackCapacity
    self.callStackCapacity = callStackCapacity
    self.standardOutput = standardOutput
  }

  /// Loads the given module.
  ///
  /// - Parameter module: A VIL module.
  public mutating func load(module: Module) throws {
    if modules[module.id] != nil { return }

    // Register the module and assign an address to each of its functions.
    modules[module.id] = module
    for (name, fun) in module.functions where fun.entry != nil {
      if funTable[name] != nil { throw RuntimeError(message: "duplicate symbol '\(name)'") }
      funTable[name] = (module.id, name)
    }

    // Register the witness tables in the module.
    viewWitnessTables.append(contentsOf: module.viewWitnessTables)
  }

  /// Starts the interpreter.
  public mutating func start() -> Int {
    // Create the main thread.
    activeThreadID = 0
    threads = [
      0: VirtualThread(
        id: 0,
        regiterStackCapacity: registerStackCapacity,
        callStackCapacity: callStackCapacity)
    ]

    // Invoke the program's entry point.
    let main = funTable["main"] ?? fatalError("no main function")
    invoke(
      funName: main.funName,
      in: modules[main.moduleID]!,
      threadID: 0,
      callerAddr: nil,
      args: [],
      returnKey: nil)
    run()

    /// Deinitialize all threads.
    for i in threads.keys {
      threads[i]!.registerStack.deinitialize()
      threads[i]!.callStack.deinitialize()
    }
    threads = [:]

    // FIXME
    // return status
    return 42
  }

  /// The active thread.
  private var activeThread: VirtualThread {
    get { threads[activeThreadID]! }
    _modify { yield &threads[activeThreadID]! }
  }

  /// The set of threads that are ready to be resumed.
  private var resumableThreads: [VirtualThread.ID] {
    return threads.keys.filter({ (key) -> Bool in
      (threads[key]!.programCounter != nil) && (threadDependencyGraph.dependencies[key] == nil)
    })
  }

  /// Invokes a VIL function on the given thread.
  ///
  /// This method implements the prologue of a function call, configuring the stack and the
  /// registers beforing jumping into the callee.
  ///
  /// The prologue consists of the following actions:
  /// - Allocate unassigned space on the stack register to store the return value.
  /// - Push a new frame onto both the register stack and the call stack.
  /// - Store the address of the caller so that the program counter can be restored later.
  /// - Assign the value of each argument to their respective register.
  /// - Move the program counter into the callee.
  ///
  /// The epilogue of the call is implemented by the `return` instruction.
  ///
  /// - Parameters:
  ///   - funName: The name of a VIL function.
  ///   - module: The ID of the module defining the specified function.
  ///   - threadID: The identifier of the thread on which the specified function is invoked. The
  ///     thread must already exist.
  ///   - callerAddr: The address of the `apply` instruction that triggered the call. `callerAddr`
  ///     cannot be `.null` unless the invoked function is the thread's entry point.
  ///   - args: The arguments of the function.
  ///   - returnKey: A key identifying the register in which the result of the call must be stored.
  private mutating func invoke<Arguments>(
    funName: String,
    in module: Module,
    threadID: VirtualThread.ID,
    callerAddr: InstAddr,
    args: Arguments,
    returnKey: RegisterTableKey?
  ) where Arguments: Sequence, Arguments.Element == RuntimeValue {
    let fun = module.functions[funName]!
    let entry = fun.entry ?? fatalError("function '\(fun.name)' has no entry block")

    // Reserve the return register, if any.
    if let key = returnKey {
      let valueWT = valueWitnessTables[valueWitnessTableKey(of: fun.type.retType!).index]
      threads[threadID]!.registerStack.reserve(
        byteCount: valueWT.size, alignedAt: valueWT.alignment, forKey: key)
    }

    // Push a new from onto the register stack and the call stack.
    threads[threadID]!.callStack.pushFrame()
    threads[threadID]!.registerStack.pushFrame()
    threads[threadID]!.registerStack.assign(native: callerAddr, to: .callerAddr)

    for (arg, param) in zip(args, module.blocks[entry].params) {
      threads[threadID]!.registerStack.assign(value: arg, to: .value(Operand(param)))
    }

    // Jump into the callee.
    threads[threadID]!.programCounter = module.blocks[entry].instructions.isEmpty
      ? nil
      : (moduleID: module.id, block: entry, offset: 0)
  }

  /// Notify all threads that the given thread has terminated.
  private mutating func notifyTermination(of supplier: VirtualThread.ID) {
    guard let dependents = threadDependencyGraph.reverseDependencies[supplier] else { return }
    for dependent in dependents {
      threadDependencyGraph.removeDependency(dependent: dependent, supplier: supplier)
    }
  }

  /// Transfers control to an unblocked thread.
  private mutating func transferControl() {
    if threads.count > 1 {
      let i = resumableThreads.randomElement()
      assert(i != nil, "all threads are blocked")
      activeThreadID = i!
    } else {
      assert(threads[0] != nil)
      activeThreadID = 0
    }
  }

  /// Runs the interpreter until there are no more instructions to evaluate.
  private mutating func run() {
//    while let inst = load(instAddr: activeThread.programCounter) {
//      switch inst {
//      case let inst as AllocExistentialInst:
//        eval(inst: inst)
//      case let inst as AllocStackInst:
//        eval(inst: inst)
//      case let inst as ApplyInst:
//        eval(inst: inst)
//      case let inst as AsyncInst:
//        eval(inst: inst)
//      case let inst as AwaitInst:
//        eval(inst: inst)
//      case let inst as BranchInst:
//        eval(inst: inst)
//      case let inst as CheckedCastAddrInst:
//        eval(inst: inst)
//      case let inst as CondBranchInst:
//        eval(inst: inst)
//      case let inst as CopyAddrInst:
//        eval(inst: inst)
//      case let inst as EqualAddrInst:
//        eval(inst: inst)
//      case let inst as HaltInst:
//        eval(inst: inst)
//      case let inst as LoadInst:
//        eval(inst: inst)
//      case let inst as CopyExistentialInst:
//        eval(inst: inst)
//      case let inst as ProjectExistentialAddrInst:
//        eval(inst: inst)
//      case let inst as PartialApplyInst:
//        eval(inst: inst)
//      case let inst as RecordInst:
//        eval(inst: inst)
//      case let inst as RecordMemberInst:
//        eval(inst: inst)
//      case let inst as RecordMemberAddrInst:
//        eval(inst: inst)
//      case let inst as RetInst:
//        eval(inst: inst)
//      case let inst as StoreInst:
//        eval(inst: inst)
//      case let inst as ThinToThickInst:
//        eval(inst: inst)
//      default:
//        fatalError("unsupported instruction \(inst)")
//      }
//    }
  }

//  private mutating func eval(inst: AllocExistentialInst) {
//    let containerAddr = eval(value: inst.container).open(as: ValueAddr.self)
//    let tableKey = valueWitnessTableKey(of: inst.witness)
//    let table = valueWitnessTables[tableKey.index]
//
//    let addr = withValue(
//      at: containerAddr,
//      boundTo: ExistentialContainer.self,
//      { (container) -> ValueAddr in
//        // Note: This precondition assumes that newly allocated memory is always zero-initialized.
//        // FIXME
//        // precondition(container.witnessKey == .zero, "container is not empty")
//        container.witnessKey = tableKey
//
//        if ExistentialContainer.holdsInPayload(byteCount: table.size, alignedAt: table.alignment) {
//          container.payload = (0, 0, 0)
//          return containerAddr
//        } else {
//          let ptr = UnsafeMutableRawPointer.allocate(
//            byteCount: table.size, alignment: table.alignment)
//          container.payload = (Int(bitPattern: ptr), table.size, 0)
//          return .heap(ptr)
//        }
//      })
//
//    activeThread.registerStack.assign(native: addr, to: .value(inst))
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: AllocStackInst) {
//    precondition(!activeThread.callStack.isEmpty, "allocation outside of a call frame")
//    let addr = activeThread.callStack.allocate(type: inst.allocType, layout: layout)
//    activeThread.registerStack.assign(native: addr, to: .value(inst))
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: ApplyInst) {
//    // The callee is either a function literal identifying a built-in or a thin function, or a
//    // reference to a register that holds a thick function.
//    switch inst.callee {
//    case let literal as BuiltinFunRef:
//      let callee = BuiltinFunction(literal: literal)
//        ?< fatalError("no built-in function named '\(literal.decl.name)'")
//      let args = inst.args.map({ eval(value: $0) })
//
//      let result = callee.apply(to: Array(args), in: &self)
//      activeThread.registerStack.assign(value: result, to: .value(inst))
//      increment(counter: &activeThread.programCounter)
//
//    case let literal as FunRef:
//      let args = inst.args.map({ eval(value: $0) })
//      invoke(
//        fun: functions[funTable[literal.name]!],
//        threadID: activeThreadID,
//        callerAddr: activeThread.programCounter,
//        args: args,
//        returnKey: .value(inst))
//
//    default:
//      var thick = eval(value: inst.callee).open(as: ThickFunction.self)
//      var args = Deque<RuntimeValue>()
//      for value in inst.args {
//        args.append(eval(value: value))
//      }
//
//      var fun: VILFun?
//      loop:while true {
//        // Unpack the environment.
//        if let env = thick.env {
//          args.prepend(contentsOf: unpackEnvironment(env: env))
//        }
//
//        // Unroll the chain of delegators.
//        switch thick.delegator {
//        case .thin(let index):
//          fun = functions[index]
//          break loop
//        case .thick(let ptr):
//          thick = ptr.assumingMemoryBound(to: ThickFunction.self).pointee
//        }
//      }
//
//      invoke(
//        fun: fun!,
//        threadID: activeThreadID,
//        callerAddr: activeThread.programCounter,
//        args: args,
//        returnKey: .value(inst))
//    }
//  }
//
//  private mutating func eval(inst: AsyncInst) {
//    var args: [RuntimeValue] = []
//    for value in inst.captures {
//      args.append(eval(value: value))
//    }
//
//    // Create a new thread and invoke the async expression on it.
//    let supplierID = threadIDFactory.makeID()
//    threads[supplierID] = VirtualThread(
//      id: supplierID,
//      regiterStackCapacity: registerStackCapacity,
//      callStackCapacity: callStackCapacity)
//
//    invoke(
//      fun: functions[funTable[inst.ref.name]!],
//      threadID: supplierID,
//      callerAddr: .null,
//      args: args,
//      returnKey: nil)
//
//    // Mark the active thread as a dependent of the thread executing the async expression and store
//    // the latter's ID into a register.
//    threadDependencyGraph.insertDependency(dependent: activeThreadID, supplier: supplierID)
//    activeThread.registerStack.assign(native: supplierID, to: .value(inst))
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: AwaitInst) {
//    let supplierID = eval(value: inst.value).open(as: VirtualThread.ID.self)
//    guard let result = threads[supplierID]!.result else {
//      // The async expression is still running; let's transfer control.
//      transferControl()
//      return
//    }
//
//    threadDependencyGraph.removeDependency(dependent: activeThreadID, supplier: supplierID)
//    result.withUnsafeBytes({ buffer in
//      activeThread.registerStack.assign(contentsOf: buffer, to: .value(inst))
//    })
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: BranchInst) {
//    // FIXME: Handle block arguments.
//    assert(inst.operands.isEmpty)
//    move(counter: &activeThread.programCounter, atStartOf: inst.dest)
//  }
//
//  private mutating func eval(inst: CheckedCastAddrInst) {
//    // A cast from a concrete type to another type can only be successful if it's an identity cast
//    // or an upcast, if the target is existential. Either way, such an operation can be checked
//    // statically and must be optimized away during VIL code emission.
//    precondition(inst.source.type.isExistential, "unreachable: source value is not existential")
//
//    let sourceAddr = eval(value: inst.source).open(as: ValueAddr.self)
//    let sourceWitnessKey = withValue(
//      at: sourceAddr, boundTo: ExistentialContainer.self, { source in
//        source.witnessKey
//      })
//
//    // If the target type is existential, we must test subtyping dynamically. Otherwise, we can
//    // open the source's existential container and return the package's address.
//    if inst.type.isExistential {
//      fatalError("not implemented")
//    }
//
//    let castAddr: ValueAddr
//    let targetWitnessKey = valueWitnessTableKey(of: inst.type.object)
//
//    if sourceWitnessKey == targetWitnessKey {
//      let table = valueWitnessTables[targetWitnessKey.index]
//      if ExistentialContainer.holdsInPayload(byteCount: table.size, alignedAt: table.alignment) {
//        castAddr = sourceAddr
//      } else {
//        castAddr = withValue(at: sourceAddr, boundTo: ExistentialContainer.self, { source in
//          ValueAddr.heap(UnsafeMutableRawPointer(bitPattern: source.payload.0)!)
//        })
//      }
//    } else {
//      castAddr = .null
//    }
//
//    activeThread.registerStack.assign(native: castAddr, to: .value(inst))
//    increment(counter: &activeThread.programCounter)
//  }
//
//  public mutating func eval(inst: CondBranchInst) {
//    let cond = eval(value: inst.cond).open(as: Bool.self)
//
//    // FIXME: Handle block arguments.
//    assert(inst.thenArgs.isEmpty && inst.elseArgs.isEmpty)
//
//    if cond {
//      move(counter: &activeThread.programCounter, atStartOf: inst.thenDest)
//    } else {
//      move(counter: &activeThread.programCounter, atStartOf: inst.elseDest)
//    }
//  }
//
//  private mutating func eval(inst: CopyAddrInst) {
//    let dst = eval(value: inst.target).open(as: ValueAddr.self)
//    let src = eval(value: inst.source).open(as: ValueAddr.self)
//
//    let byteCount = layout.size(of: inst.source.type.object)
//    guard byteCount > 0 else { return }
//
//    switch (dst, src) {
//    case (.stack(let dst), .stack(let src)):
//      activeThread.callStack.copyMemory(to: dst, from: src, count: byteCount)
//
//    case (.stack(let dst), .heap(let src)):
//      activeThread.callStack.withUnsafeMutableRawPointer(from: dst, { ptr in
//        ptr.copyMemory(from: src, byteCount: byteCount)
//      })
//
//    case (.heap(let dst), .stack(let src)):
//      let buffer = activeThread.callStack.unsafeRawBufferPointer(from: src, byteCount: byteCount)
//      dst.copyMemory(from: buffer.baseAddress!, byteCount: byteCount)
//
//    case (.heap(let dst), .heap(let src)):
//      dst.copyMemory(from: src, byteCount: byteCount)
//
//    default:
//      fatalError("null address")
//    }
//
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: EqualAddrInst) {
//    let lhs = eval(value: inst.lhs).open(as: ValueAddr.self)
//    let rhs = eval(value: inst.rhs).open(as: ValueAddr.self)
//    activeThread.registerStack.assign(native: lhs == rhs, to: .value(inst))
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: HaltInst) {
//    activeThreadID = 0
//    for i in threads.keys {
//      threads[i]!.programCounter = .null
//    }
//  }
//
//  private mutating func eval(inst: LoadInst) {
//    let src = eval(value: inst.source).open(as: ValueAddr.self)
//    load(byteCount: layout.size(of: inst.type), from: src, into: .value(inst))
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: CopyExistentialInst) {
//    let container = eval(value: inst.container).open(as: ExistentialContainer.self)
//
//    // Make sure the has the expected witness.
//    precondition(container.witnessKey != .zero, "container is empty")
//    precondition(container.witnessKey == valueWitnessTableKey(of: inst.type), "bad witness")
//    let table = valueWitnessTables[container.witnessKey.index]
//
//    // Load the packaged value, either from the payload of the existential, or from the address
//    // stored in the container.
//    if ExistentialContainer.holdsInPayload(byteCount: table.size, alignedAt: table.alignment) {
//      withUnsafeBytes(of: container.payload, { (buffer) -> Void in
//        activeThread.registerStack.assign(
//          contentsOf: UnsafeRawBufferPointer(rebasing: buffer[0 ..< table.size]),
//          to: .value(inst))
//      })
//    } else {
//      let buffer = UnsafeRawBufferPointer(
//        start: UnsafeRawPointer(bitPattern: container.payload.0), count: container.payload.1)
//      activeThread.registerStack.assign(contentsOf: buffer, to: .value(inst))
//    }
//
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: ProjectExistentialAddrInst) {
//    let containerAddr = eval(value: inst.container).open(as: ValueAddr.self)
//    let tableKey = valueWitnessTableKey(of: inst.type.object)
//    let table = valueWitnessTables[tableKey.index]
//
//    let addr = withValue(
//      at: containerAddr,
//      boundTo: ExistentialContainer.self,
//      { (container) -> ValueAddr in
//        precondition(container.witnessKey != .zero, "container is empty")
//        precondition(container.witnessKey == tableKey, "bad witness")
//
//        if ExistentialContainer.holdsInPayload(byteCount: table.size, alignedAt: table.alignment) {
//          return containerAddr
//        } else {
//          return .heap(UnsafeMutableRawPointer(bitPattern: container.payload.0)!)
//        }
//      })
//
//    activeThread.registerStack.assign(native: addr, to: .value(inst))
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: PartialApplyInst) {
//    func _makeEnvironment() -> UnsafeRawPointer {
//      var args: [RuntimeValue] = []
//      var params: [VILType] = []
//      for i in 0 ..< inst.partialArgs.count {
//        args.append(eval(value: inst.partialArgs[i]))
//        params.append(inst.partialArgs[i].type)
//      }
//      return makeEnvironment(args: args, params: params)
//    }
//
//    switch inst.delegator {
//    case is BuiltinFunRef:
//      fatalError("cannot partially apply built-in function")
//
//    case let literal as FunRef:
//      let delegator = funTable[literal.name]!
//      let env = _makeEnvironment()
//      let partial = ThickFunction(delegator: .thin(delegator), env: env)
//      activeThread.registerStack.assign(native: partial, to: .value(inst))
//
//    default:
//      fatalError("not implemented")
//    }
//
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: RecordInst) {
//    let valueWT = valueWitnessTables[valueWitnessTableKey(of: inst.type).index]
//    activeThread.registerStack.reserve(
//      byteCount: valueWT.size, alignedAt: valueWT.alignment, forKey: .value(inst))
//    activeThread.registerStack.assignNoAlloc(
//      contentsOf: UnsafeRawBufferPointer(start: nil, count: 0), to: .value(inst))
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: RecordMemberInst) {
//    let record = eval(value: inst.record)
//    let offset = layout.offset(of: inst.memberDecl.name, in: inst.record.type)
//      ?< fatalError("failed to compute member offset")
//
//    let byteCount = layout.size(of: inst.type)
//    let member = record.withUnsafeBytes({ bytes in
//      return RuntimeValue(
//        borrowing: UnsafeRawBufferPointer(rebasing: bytes[offset ..< (offset + byteCount)]))
//    })
//
//    activeThread.registerStack.assign(value: member, to: .value(inst))
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: RecordMemberAddrInst) {
//    let base = eval(value: inst.record).open(as: ValueAddr.self)
//    let offset = layout.offset(of: inst.memberDecl.name, in: inst.record.type.object)
//      ?< fatalError("failed to compute member offset")
//
//    activeThread.registerStack.assign(native: base.advanced(by: offset), to: .value(inst))
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: RetInst) {
//    // First, we need to load the caller's address from the register stack to get the name of the
//    // return register in the caller's frame. If there isn't any, then the thread just terminated
//    // and we'll store the result as a field.
//    let callerAddr = activeThread.registerStack.load(InstAddr.self, forKey: .callerAddr)
//
//    let result = eval(value: inst.value)
//    activeThread.registerStack.removeFrame()
//    activeThread.callStack.removeFrame()
//
//    if callerAddr != .null {
//      // The return value may be stored in memory that has just been deallocated from the register
//      // stack. We can assume, however, that the return register has been reserved in the previous
//      // frame when the function was invoked. Hence, that memory won't be overridden if we only
//      // move it to the return register without allocating anything else.
//      let callerInst = load(instAddr: callerAddr) as? ApplyInst
//        ?< fatalError("bad caller address")
//
//      result.withUnsafeBytes({ bytes in
//        activeThread.registerStack.assignNoAlloc(contentsOf: bytes, to: .value(callerInst))
//      })
//      activeThread.programCounter = callerAddr
//      increment(counter: &activeThread.programCounter)
//    } else {
//      activeThread.result = result.withUnsafeBytes(RuntimeValue.init(copying:))
//      activeThread.programCounter = .null
//
//      // Deallocate the thread's memory.
//      activeThread.registerStack.deinitialize()
//      activeThread.callStack.deinitialize()
//
//      // If we're not running the main thread, we must notify all possible dependents that we're
//      // done and transfer control to another thread. Otherwise, we must terminate the program,
//      // aborting all other threads.
//      if activeThreadID != 0 {
//        notifyTermination(of: activeThreadID)
//        transferControl()
//      } else {
//        // FIXME: Should we do something about threads that didn't terminate?
//      }
//    }
//  }
//
//  private mutating func eval(inst: StoreInst) {
//    let dest = eval(value: inst.target).open(as: ValueAddr.self)
//    store(value: eval(value: inst.value), at: dest)
//    increment(counter: &activeThread.programCounter)
//  }
//
//  private mutating func eval(inst: ThinToThickInst) {
//    let thick = ThickFunction(thin: funTable[inst.ref.name]!)
//    activeThread.registerStack.assign(native: thick, to: .value(inst))
//    increment(counter: &activeThread.programCounter)
//  }
//
//  /// Evaluates a value.
//  private mutating func eval(value: Value) -> RuntimeValue {
//    switch value {
//    case is UnitValue:
//      return .unit
//
//    case is NullAddr:
//      return RuntimeValue(copyingRawBytesOf: ValueAddr.null)
//
//    case let literal as IntLiteralValue:
//      return RuntimeValue(copyingRawBytesOf: literal.value)
//
//    case is BuiltinFunRef, is FunRef:
//      fatalError("unexpected function literal")
//
//    default:
//      return RuntimeValue(
//        borrowing: activeThread.registerStack.unsafeRawBufferPointer(forKey: .value(value)))
//    }
//  }

  /// Increments the given program counter.
  private func increment(counter: inout InstAddr) {
    counter!.offset += 1
  }

  /// Moves the given program counter at the beginning of the specified block.
  private func move(counter: inout InstAddr, atStartOf block: BasicBlockIndex, in module: String) {
    counter = (moduleID: module, block: block, offset: 0)
  }

  /// Dereferences an instruction address.
  private func load(instAddr: InstAddr) -> Inst? {
    guard let (moduleID, block, offset) = instAddr else { return nil }

    let module = modules[moduleID]!
    return module.instructions[module.blocks[block].instructions[offset]]
  }

  /// Loads `byteCount` bytes from the given address into a register.
  private mutating func load(byteCount: Int, from src: ValueAddr, into key: RegisterTableKey) {
    switch src {
    case .null:
      fatalError("null address")

    case .stack(let offset):
      let buffer = threads[offset.threadID]!.callStack
        .unsafeRawBufferPointer(from: offset, byteCount: byteCount)
      activeThread.registerStack.assign(contentsOf: buffer, to: key)

    case .heap(let ptr):
      UnsafeMutableRawBufferPointer.allocate(byteCount: 1, alignment: 1).copyBytes(from: [1])
      activeThread.registerStack.assign(
        contentsOf: UnsafeRawBufferPointer(start: ptr, count: byteCount), to: key)
    }
  }

  /// Stores a value at the given address.
  private mutating func store(value: RuntimeValue, at addr: ValueAddr) {
    switch addr {
    case .null:
      fatalError("null address")

    case .stack(let offset):
      threads[offset.threadID]!.callStack.withUnsafeMutableRawPointer(
        from: offset,
        value.copyMemory(to:))

    case .heap(let ptr):
      value.copyMemory(to: ptr)
    }
  }

  private mutating func withValue<T, R>(
    at addr: ValueAddr, boundTo: T.Type, _ body: (inout T) -> R
  ) -> R {
    switch addr {
    case .null:
      fatalError("null address")

    case .stack(let offset):
      return threads[offset.threadID]!.callStack
        .withUnsafeMutableRawPointer(from: offset, { ptr in
          body(&ptr.assumingMemoryBound(to: T.self).pointee)
        })

    case .heap(let ptr):
      return body(&ptr.assumingMemoryBound(to: T.self).pointee)
    }
  }

  /// Returns the key of the value witness table of the given type, creating it if necessary.
  private mutating func valueWitnessTableKey(of type: VILType) -> ValueWitnessTableKey {
    assert(type.isObject)
    if let key = typeToValueWitnessTableKey[type.valType] {
      return key
    }

    valueWitnessTables.append(
      ValueWitnessTable(
        type: type,
        size: layout.size(of: type),
        stride: layout.stride(of: type),
        alignment: layout.alignment(of: type)))

    let key = ValueWitnessTableKey(rawValue: valueWitnessTables.count)
    typeToValueWitnessTableKey[type.valType] = key
    return key
  }

  private mutating func makeEnvironment(
    args: [RuntimeValue], params: [VILType]
  ) -> UnsafeRawPointer {
    // Compute the properties of the environment's memory layout.
    let payloadOffsets = layout.offsets(in: params)
    let align = params.reduce(8, { max($0, layout.alignment(of: $1)) })
    let headerByteCount = 8 + MemoryLayout<ValueWitnessTableKey>.stride * params.count
    let offset = nextOffset(alignedAt: align, from: headerByteCount)

    // Allocate the environment buffer.
    let env = UnsafeMutableRawPointer.allocate(
      byteCount: offset + payloadOffsets.last!, alignment: align)
    let witnessTableKeys = env
      .advanced(by: 8)
      .assumingMemoryBound(to: ValueWitnessTableKey.self)

    env.assumingMemoryBound(to: UInt32.self)
      .assign(repeating: UInt32(truncatingIfNeeded: params.count), count: 1)
    env.advanced(by: 4).assumingMemoryBound(to: UInt32.self)
      .assign(repeating: UInt32(truncatingIfNeeded: align), count: 1)

    // Pack the arguments into the environment.
    for i in 0 ..< params.count {
      witnessTableKeys[i] = valueWitnessTableKey(of: params[i])
      args[i].copyMemory(to: env.advanced(by: offset + payloadOffsets[i]))
    }

    return UnsafeRawPointer(env)
  }

  private func unpackEnvironment(env: UnsafeRawPointer) -> [RuntimeValue] {
    // Compute the properties of the environment's memory layout.
    let count = Int(truncatingIfNeeded: env.load(fromByteOffset: 0, as: UInt32.self))
    let align = Int(truncatingIfNeeded: env.load(fromByteOffset: 4, as: UInt32.self))
    let headerByteCount = 8 + MemoryLayout<ValueWitnessTableKey>.stride * count
    let witnessTableKeys = env
      .advanced(by: 8)
      .assumingMemoryBound(to: ValueWitnessTableKey.self)

    // Unpack the payload.
    var offset = nextOffset(alignedAt: align, from: headerByteCount)
    var values: [RuntimeValue] = []
    for i in 0 ..< count {
      let table = valueWitnessTables[witnessTableKeys[i].index]
      values.append(
        RuntimeValue(borrowing: UnsafeRawBufferPointer(start: env + offset, count: table.size)))
      offset += nextOffset(alignedAt: align, from: offset + table.size)
    }

    return values
  }

}
