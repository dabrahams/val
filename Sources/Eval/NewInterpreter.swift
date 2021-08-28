import class Foundation.FileHandle
import Basic
import DequeModule
import VIL

import class AST.ValType

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
public struct NewInterpreter {

  /// A key identifying a value witness table.
  struct ValueWitnessTableKey: Equatable {

    /// The underlying index in the array of witness tables, offset by one.
    fileprivate var rawValue: Int

    fileprivate var index: Int { rawValue - 1 }

    static var zero: ValueWitnessTableKey { ValueWitnessTableKey(rawValue: 0) }

  }

  /// The data layout used to calculate type layout information.
  private var layout = DataLayout()

  /// The functions loaded into the interpreter.
  private var functions: [String: Function] = [:]

  /// The view witness tables loaded into the interpreter.
  private var viewWitnessTables: [ViewWitnessTable] = []

  /// The value witness tables loaded into the interpreter.
  private var valueWitnessTables: [ValueWitnessTable] = []

  /// A table mapping VIL types to their value witness table.
  private var typeToValueWitnessTableKey: ReferenceTable<ValType, ValueWitnessTableKey> = [:]

  /// The register stack of the interpreter.
  private var registerStack: RegisterStack

  /// The call stack of the interpreter.
  private var callStack: CallStack

  /// The current program counter of the interpreter.
  private var pc = InstAddr.null

  /// The standard output of the interpreter.
  let standardOutput: FileHandle

  /// The status of the interpreter.
  var status: Int = 0

  /// Initializes a new interpreter.
  ///
  /// - Parameters:
  ///   - regiterStackCapacity: The initial capacity of the register stack, in bytes.
  ///   - callStackCapacity: The initial capacity of the call stack, in bytes.
  public init(
    regiterStackCapacity: Int = 8_388_608,
    callStackCapacity: Int = 1_048_576,
    standardOutput: FileHandle = .standardOutput
  ) {
    self.registerStack = RegisterStack(initialCapacity: regiterStackCapacity)
    self.callStack = CallStack(initialCapacity: callStackCapacity)
    self.standardOutput = standardOutput
  }

  /// Deinitializes the interpreter.
  public mutating func deinitialize() {
    registerStack.deinitialize()
    callStack.deinitialize()
  }

  /// Loads the given module.
  ///
  /// - Parameter module: A VIL module.
  public mutating func load(module: Module) throws {
    try functions.merge(module.functions, uniquingKeysWith: { (lhs, rhs) throws -> Function in
      if lhs.blocks.isEmpty {
        return rhs
      } else if rhs.blocks.isEmpty {
        return lhs
      } else {
        throw RuntimeError(message: "duplicate symbol '\(lhs)'")
      }
    })
    viewWitnessTables.append(contentsOf: module.viewWitnessTables)
  }

  /// Starts the interpreter.
  public mutating func start() -> Int {
    let main = functions["main"] ?< fatalError("no main function")
    invoke(function: main, callerAddr: .null, args: [], returnKey: nil)
    run()

    return status
  }

  private mutating func run() {
    while let inst = load(instAddr: pc) {
      switch inst {
      case let inst as AllocExistentialInst:
        eval(inst: inst)
      case let inst as AllocStackInst:
        eval(inst: inst)
      case let inst as ApplyInst:
        eval(inst: inst)
      case let inst as AsyncInst:
        eval(inst: inst)
      case let inst as BranchInst:
        eval(inst: inst)
      case let inst as CheckedCastAddrInst:
        eval(inst: inst)
      case let inst as CondBranchInst:
        eval(inst: inst)
      case let inst as CopyAddrInst:
        eval(inst: inst)
      case let inst as EqualAddrInst:
        eval(inst: inst)
      case let inst as HaltInst:
        eval(inst: inst)
      case let inst as LoadInst:
        eval(inst: inst)
      case let inst as OpenExistentialInst:
        eval(inst: inst)
      case let inst as OpenExistentialAddrInst:
        eval(inst: inst)
      case let inst as PartialApplyInst:
        eval(inst: inst)
      case let inst as RecordInst:
        eval(inst: inst)
      case let inst as RecordMemberInst:
        eval(inst: inst)
      case let inst as RecordMemberAddrInst:
        eval(inst: inst)
      case let inst as RetInst:
        eval(inst: inst)
      case let inst as StoreInst:
        eval(inst: inst)
      case let inst as ThinToThickInst:
        eval(inst: inst)
      default:
        fatalError("unsupported instruction \(inst)")
      }
    }
  }

  /// Invokes the given VIL function.
  private mutating func invoke<Arguments>(
    function: Function,
    callerAddr: InstAddr,
    args: Arguments,
    returnKey: RegisterTableKey?
  ) where Arguments: Sequence, Arguments.Element == NewRuntimeValue {
    let entryID = function.entryID
      ?< fatalError("function '\(function.name)' has no entry block")

    // Reserve the return register, if any.
    if let key = returnKey {
      let valueWT = valueWitnessTables[valueWitnessTableKey(of: function.type.retType).index]
      registerStack.reserve(byteCount: valueWT.size, alignedAt: valueWT.alignment, forKey: key)
    }

    // Push a new from onto the register stack and the call stack.
    callStack.pushFrame()
    registerStack.pushFrame()
    registerStack.assign(value: callerAddr, to: .callerAddr)
    for (arg, reg) in zip(args, function.blocks[entryID]!.arguments) {
      arg.assign(to: .value(reg), in: &registerStack)
    }

    // Jump into the callee.
    pc = InstAddr(function: function, blockID: entryID, offset: 0)
  }

  private mutating func eval(inst: AllocExistentialInst) {
    let containerAddr = eval(value: inst.container).open(as: ValueAddr.self)
    let tableKey = valueWitnessTableKey(of: inst.witness)
    let table = valueWitnessTables[tableKey.index]

    let addr = withValue(
      at: containerAddr,
      boundTo: ExistentialContainer.self,
      { (container) -> ValueAddr in
        // Note: This precondition assumes that newly allocated memory is always zero-initialized.
        // precondition(container.witnessKey == .zero, "container is not empty")
        container.witnessKey = tableKey

        if ExistentialContainer.holdsInPayload(byteCount: table.size, alignedAt: table.alignment) {
          container.payload = (0, 0, 0)
          return containerAddr
        } else {
          let ptr = UnsafeMutableRawPointer.allocate(
            byteCount: table.size, alignment: table.alignment)
          container.payload = (Int(bitPattern: ptr), table.size, 0)
          return .heap(ptr)
        }
      })

    addr.assign(to: .value(inst), in: &registerStack)
    pc.offset += 1
  }

  private mutating func eval(inst: AllocStackInst) {
    precondition(!callStack.isEmpty, "allocation outside of a call frame")
    let addr = callStack.allocate(type: inst.allocatedType, layout: layout)
    addr.assign(to: .value(inst), in: &registerStack)
    pc.offset += 1
  }

  private mutating func eval(inst: ApplyInst) {
    let callee = eval(value: inst.fun)

    var args = Deque<NewRuntimeValue>()
    for value in inst.args {
      args.append(eval(value: value))
    }

    // The callee is either a function literal or a reference to a register that holds a thick
    // function. In the former case, `eval(value:)` will have returned a function value. In the
    // latter case, it will have returned a buffer that we should load as a thick function.
    switch callee {
    case let callee as BuiltinFunction:
      let result = callee.apply(to: Array(args), in: &self)
      result.assign(to: .value(inst), in: &registerStack)
      pc.offset += 1

    case let callee as ThinFunction:
      let function = Unmanaged<Function>.fromOpaque(callee.ptr).takeUnretainedValue()
      invoke(function: function, callerAddr: pc, args: args, returnKey: .value(inst))

    default:
      var thick = callee.open(as: ThickFunction.self)
      var function: Function?
      loop:while true {
        // Unpack the environment.
        if let env = thick.env {
          args.prepend(contentsOf: unpackEnvironment(env: env))
        }

        // Unroll the chain of delegators.
        switch thick.delegator {
        case .thin(let ptr):
          function = Unmanaged<Function>.fromOpaque(ptr).takeUnretainedValue()
          break loop
        case .thick(let addr):
          withValue(at: addr, boundTo: ThickFunction.self, { thick = $0 })
        }
      }

      invoke(function: function!, callerAddr: pc, args: args, returnKey: .value(inst))
      return
    }
  }

  private mutating func eval(inst: AsyncInst) {
    var args: [NewRuntimeValue] = []
    var params: [VILType] = []
    for i in 0 ..< inst.args.count {
      args.append(eval(value: inst.args[i]))
      params.append(inst.args[i].type)
    }
    let env = makeEnvironment(args: args, params: params)

    let thinPtr = Unmanaged.passUnretained(inst.fun).toOpaque()
    let partial = ThickFunction(delegator: .thin(thinPtr), env: env)

    pc.offset += 1
  }

  private mutating func eval(inst: BranchInst) {
    // FIXME: Handle block arguments.
    assert(inst.args.isEmpty)
    pc.blockID = inst.dest
    pc.offset = 0
  }

  private mutating func eval(inst: CheckedCastAddrInst) {
    // A cast from a concrete type to another type can only be successful if it's an identity cast
    // or an upcast, if the target is existential. Either way, such an operation can be checked
    // statically and should be optimized away during VIL code emission.
    precondition(inst.source.type.isExistential, "unreachable: source value is not existential")

    let sourceAddr = eval(value: inst.source).open(as: ValueAddr.self)
    let sourceWitnessKey = withValue(
      at: sourceAddr, boundTo: ExistentialContainer.self, { source in
        source.witnessKey
      })

    // If the target type is existential, we must test subtyping dynamically. Otherwise, we can
    // open the source's existential container and return the package's address.
    if inst.type.isExistential {
      fatalError("not implemented")
    }

    let castAddr: ValueAddr
    let targetWitnessKey = valueWitnessTableKey(of: inst.type.object)

    if sourceWitnessKey == targetWitnessKey {
      let table = valueWitnessTables[targetWitnessKey.index]
      if ExistentialContainer.holdsInPayload(byteCount: table.size, alignedAt: table.alignment) {
        castAddr = sourceAddr
      } else {
        castAddr = withValue(at: sourceAddr, boundTo: ExistentialContainer.self, { source in
          ValueAddr.heap(UnsafeMutableRawPointer(bitPattern: source.payload.0)!)
        })
      }
    } else {
      castAddr = .null
    }

    castAddr.assign(to: .value(inst), in: &registerStack)
    pc.offset += 1
  }

  public mutating func eval(inst: CondBranchInst) {
    let cond = eval(value: inst.cond).open(as: Bool.self)

    // FIXME: Handle block arguments.
    assert(inst.thenArgs.isEmpty && inst.elseArgs.isEmpty)

    if cond {
      pc.blockID = inst.thenDest
      pc.offset = 0
    } else {
      pc.blockID = inst.elseDest
      pc.offset = 0
    }
  }

  private mutating func eval(inst: CopyAddrInst) {
    let dst = eval(value: inst.dest).open(as: ValueAddr.self)
    let src = eval(value: inst.source).open(as: ValueAddr.self)
    let byteCount = layout.size(of: inst.source.type.object)

    switch (dst, src) {
    case (.stack(let dst), .stack(let src)):
      callStack.copyMemory(to: dst, from: src, count: byteCount)

    case (.stack(let dst), .heap(let src)):
      callStack.withUnsafeMutableRawPointer(from: dst, { ptr in
        ptr.copyMemory(from: src, byteCount: byteCount)
      })

    case (.heap(let dst), .stack(let src)):
      callStack.withUnsafeRawPointer(from: src, { ptr in
        dst.copyMemory(from: ptr, byteCount: byteCount)
      })

    case (.heap(let dst), .heap(let src)):
      dst.copyMemory(from: src, byteCount: byteCount)

    default:
      fatalError("null address")
    }

    pc.offset += 1
  }

  private mutating func eval(inst: EqualAddrInst) {
    let lhs = eval(value: inst.lhs).open(as: ValueAddr.self)
    let rhs = eval(value: inst.rhs).open(as: ValueAddr.self)
    let result = lhs == rhs
    result.assign(to: .value(inst), in: &registerStack)
    pc.offset += 1
  }

  private mutating func eval(inst: HaltInst) {
    pc = .null
  }

  private mutating func eval(inst: LoadInst) {
    let src = eval(value: inst.lvalue).open(as: ValueAddr.self)
    load(byteCount: layout.size(of: inst.type), from: src, into: .value(inst))
    pc.offset += 1
  }

  private mutating func eval(inst: OpenExistentialInst) {
    let container = eval(value: inst.container).open(as: ExistentialContainer.self)

    // Make sure the has the expected witness.
    precondition(container.witnessKey != .zero, "container is empty")
    precondition(container.witnessKey == valueWitnessTableKey(of: inst.type), "bad witness")
    let table = valueWitnessTables[container.witnessKey.index]

    // Load the packaged value, either from the payload of the existential, or from the address
    // stored in the container.
    if ExistentialContainer.holdsInPayload(byteCount: table.size, alignedAt: table.alignment) {
      withUnsafeBytes(of: container.payload, { (buffer) -> Void in
        let buffer = UnsafeRawBufferPointer(rebasing: buffer[0 ..< table.size])
        buffer.assign(to: .value(inst), in: &registerStack)
      })
    } else {
      let buffer = UnsafeRawBufferPointer(
        start: UnsafeRawPointer(bitPattern: container.payload.0), count: container.payload.1)
      buffer.assign(to: .value(inst), in: &registerStack)
    }

    pc.offset += 1
  }

  private mutating func eval(inst: OpenExistentialAddrInst) {
    let containerAddr = eval(value: inst.container).open(as: ValueAddr.self)
    let tableKey = valueWitnessTableKey(of: inst.type.object)
    let table = valueWitnessTables[tableKey.index]

    let addr = withValue(
      at: containerAddr,
      boundTo: ExistentialContainer.self,
      { (container) -> ValueAddr in
        precondition(container.witnessKey != .zero, "container is empty")
        precondition(container.witnessKey == tableKey, "bad witness")

        if ExistentialContainer.holdsInPayload(byteCount: table.size, alignedAt: table.alignment) {
          return containerAddr
        } else {
          return .heap(UnsafeMutableRawPointer(bitPattern: container.payload.0)!)
        }
      })

    addr.assign(to: .value(inst), in: &registerStack)
    pc.offset += 1
  }

  private mutating func eval(inst: PartialApplyInst) {
    let fun = eval(value: inst.fun)
    precondition(!(fun is BuiltinFunction), "cannot partially apply built-in function")

    // Create the closure's environment.
    var args: [NewRuntimeValue] = []
    var params: [VILType] = []
    for i in 0 ..< inst.args.count {
      args.append(eval(value: inst.args[i]))
      params.append(inst.args[i].type)
    }
    let env = makeEnvironment(args: args, params: params)

    if let callee = fun as? ThinFunction {
      let partial = ThickFunction(delegator: .thin(callee.ptr), env: env)
      partial.assign(to: .value(inst), in: &registerStack)
    } else {
//      var thick = fun.open(as: ThickFunction.self)
//      var function: Function?
      fatalError("not implemented")
    }

    pc.offset += 1
  }

  private mutating func eval(inst: RecordInst) {
    let valueWT = valueWitnessTables[valueWitnessTableKey(of: inst.type).index]
    registerStack.reserve(
      byteCount: valueWT.size, alignedAt: valueWT.alignment, forKey: .value(inst))
    registerStack.assignNoAlloc(
      contentsOf: UnsafeRawBufferPointer(start: nil, count: 0), to: .value(inst))
    pc.offset += 1
  }

  private mutating func eval(inst: RecordMemberInst) {
    let record = eval(value: inst.record) as! UnsafeRawBufferPointer
    let offset = layout.offset(of: inst.memberDecl.name, in: inst.record.type)
      ?< fatalError("failed to compute member offset")

    let byteCount = layout.size(of: inst.type)
    let buffer = UnsafeRawBufferPointer(rebasing: record[offset ..< (offset + byteCount)])
    buffer.assign(to: .value(inst), in: &registerStack)
    pc.offset += 1
  }

  private mutating func eval(inst: RecordMemberAddrInst) {
    let base = eval(value: inst.record).open(as: ValueAddr.self)
    let offset = layout.offset(of: inst.memberDecl.name, in: inst.record.type.object)
      ?< fatalError("failed to compute member offset")
    base.advanced(by: offset).assign(to: .value(inst), in: &registerStack)
    pc.offset += 1
  }

  private mutating func eval(inst: RetInst) {
    // First, we need to load the caller's address from the register stack to get the name of the
    // return register in the caller's frame. If there isn't any, then we just have to pop the
    // current frames and we're done.
    let callerAddr = registerStack.load(InstAddr.self, forKey: .callerAddr)
    guard callerAddr != .null else {
      registerStack.removeFrame()
      callStack.removeFrame()
      pc = .null
      return
    }

    let callerInst = load(instAddr: callerAddr) as? ApplyInst
      ?< fatalError("bad caller address")

    // The return value may be stored in memory that is about to be deallocated from the register
    // stack. We can assume, however, that the return register has been reserved in the previous
    // frame when the function was invoked. Hence, that memory won't be overridden if we only move
    // it to the return register without allocating anything else.
    let value = eval(value: inst.value)

    registerStack.removeFrame()
    callStack.removeFrame()
    value.assignNoAlloc(to: .value(callerInst), in: &registerStack)

    pc = callerAddr
    pc.offset += 1
  }

  private mutating func eval(inst: StoreInst) {
    let dest = eval(value: inst.lvalue).open(as: ValueAddr.self)
    store(value: eval(value: inst.rvalue), at: dest)
    pc.offset += 1
  }

  private mutating func eval(inst: ThinToThickInst) {
    let thick = ThickFunction(thin: functions[inst.ref.name]!)
    thick.assign(to: .value(inst), in: &registerStack)
    pc.offset += 1
  }

  /// Evaluates a value.
  private mutating func eval(value: Value) -> NewRuntimeValue {
    switch value {
    case is UnitValue:
      return UnsafeRawBufferPointer(start: nil, count: 0)

    case is NullAddr:
      return ValueAddr.null

    case let literal as IntLiteralValue:
      return literal.value

    case let literal as BuiltinFunRef:
      let fun = BuiltinFunction(literal: literal)
        ?< fatalError("no built-in function named '\(literal.decl.name)'")
      return fun

    case let literal as FunRef:
      return ThinFunction(function: functions[literal.name]!)

    default:
      return registerStack.unsafeRawBufferPointer(forKey: .value(value))
    }
  }

  /// Dereferences the basic block containing the instruction referenced by the given address.
  private func load(blockContaining addr: InstAddr) -> BasicBlock? {
    guard let ptr = addr.functionPtr else { return nil }

    let function = Unmanaged<Function>.fromOpaque(ptr).takeUnretainedValue()
    return function.blocks[addr.blockID]
  }

  /// Dereferences an instruction address.
  private func load(instAddr addr: InstAddr) -> Inst? {
    guard let block = load(blockContaining: addr) else { return nil }
    return addr.offset < block.instructions.count
      ? block.instructions[addr.offset]
      : nil
  }

  /// Loads `byteCount` bytes from the given address into a register.
  private mutating func load(byteCount: Int, from src: ValueAddr, into key: RegisterTableKey) {
    switch src {
    case .null:
      fatalError("null address")

    case .stack(let offset):
      callStack.withUnsafeRawPointer(from: offset, { ptr in
        registerStack.assign(
          contentsOf: UnsafeRawBufferPointer(start: ptr, count: byteCount), to: key)
      })

    case .heap(let ptr):
      UnsafeMutableRawBufferPointer.allocate(byteCount: 1, alignment: 1).copyBytes(from: [1])
      registerStack.assign(
        contentsOf: UnsafeRawBufferPointer(start: ptr, count: byteCount), to: key)
    }
  }

  /// Stores a value at the given address.
  private mutating func store(value: NewRuntimeValue, at addr: ValueAddr) {
    switch addr {
    case .null:
      fatalError("null address")

    case .stack(let offset):
      callStack.withUnsafeMutableRawPointer(from: offset, value.store(to:))

    case .heap(let ptr):
      addr.store(to: ptr)
    }
  }

  private mutating func withValue<T, R>(
    at addr: ValueAddr, boundTo: T.Type, _ body: (inout T) -> R
  ) -> R {
    switch addr {
    case .null:
      fatalError("null address")

    case .stack(let offset):
      return callStack.withUnsafeMutableRawPointer(from: offset, { ptr in
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
    args: [NewRuntimeValue], params: [VILType]
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
      args[i].store(to: env.advanced(by: offset + payloadOffsets[i]))
    }

    return UnsafeRawPointer(env)
  }

  private func unpackEnvironment(env: UnsafeRawPointer) -> [NewRuntimeValue] {
    // Compute the properties of the environment's memory layout.
    let count = Int(truncatingIfNeeded: env.load(fromByteOffset: 0, as: UInt32.self))
    let align = Int(truncatingIfNeeded: env.load(fromByteOffset: 4, as: UInt32.self))
    let headerByteCount = 8 + MemoryLayout<ValueWitnessTableKey>.stride * count
    let witnessTableKeys = env
      .advanced(by: 8)
      .assumingMemoryBound(to: ValueWitnessTableKey.self)

    // Unpack the payload.
    var offset = nextOffset(alignedAt: align, from: headerByteCount)
    var values: [NewRuntimeValue] = []
    for i in 0 ..< count {
      let table = valueWitnessTables[witnessTableKeys[i].index]
      values.append(UnsafeRawBufferPointer(start: env + offset, count: table.size))
      offset += nextOffset(alignedAt: align, from: offset + table.size)
    }

    return values
  }

}
