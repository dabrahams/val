import class Foundation.FileHandle

import Basic
import VIL

/// A virtual machine that interprets VIL code.
public struct NewInterpreter {

  /// The data layout used to calculate type layout information.
  private var layout = DataLayout()

  /// The functions loaded into the interpreter.
  private var functions: [String: Function] = [:]

  /// The witness tables loaded into the interpreter.
  private var witnessTables: [WitnessTable] = []

  /// The register stack of the interpreter.
  private var registerStack: RegisterStack

  /// The call stack of the interpreter.
  private var callStack: CallStack

  /// The current program counter of the interpreter.
  private var pc: InstAddr?

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
    witnessTables.append(contentsOf: module.witnessTables)
  }

  /// Starts the interpreter.
  public mutating func start() -> Int {
    let main = functions["main"] ?< fatalError("no main function")
    invoke(function: main, callerAddr: nil, args: [], returnKey: nil)
    run()

    return status
  }

  private mutating func run() {
    while let addr = pc, let inst = load(instAddr: addr) {
      switch inst {
      case let inst as AllocExistentialInst:
        eval(inst: inst)
      case let inst as AllocStackInst:
        eval(inst: inst)
      case let inst as ApplyInst:
        eval(inst: inst)
      case let inst as LoadInst:
        eval(inst: inst)
      case let inst as OpenExistentialInst:
        eval(inst: inst)
      case let inst as PartialApplyInst:
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
  private mutating func invoke(
    function: Function,
    callerAddr: InstAddr?,
    args: [NewRuntimeValue],
    returnKey: RegisterTableKey?
  ) {
    let entryID = function.entryID
      ?< fatalError("function '\(function.name)' has no entry block")

    // Reserve the return register, if any.
    if let key = returnKey {
      registerStack.reserve(registerOfType: function.type.retType, layout: layout, forKey: key)
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

    let pkgSize = layout.size(of: inst.witness)
    let pkgAlign = layout.alignment(of: inst.witness)

    let pkgAddr = withValue(
      at: containerAddr,
      boundTo: ExistentialContainer.self,
      { (container) -> ValueAddr in
        // Note: This precondition assumes that newly allocated memory is always zero-initialized.
        precondition(container.witness == nil, "container is not empty")
        container.witness = Unmanaged.passUnretained(inst.witness).toOpaque()

        if ExistentialContainer.holdsInPayload(byteCount: pkgSize, alignedAt: pkgAlign) {
          container.payload = (0, 0, 0)
          return containerAddr
        } else {
          let ptr = UnsafeMutableRawPointer.allocate(byteCount: pkgSize, alignment: pkgAlign)
          container.payload = (Int(bitPattern: ptr), pkgSize, 0)
          return .heap(ptr)
        }
      })

    pkgAddr.assign(to: .value(inst), in: &registerStack)
    pc!.offset += 1
  }

  private mutating func eval(inst: AllocStackInst) {
    precondition(!callStack.isEmpty, "allocation outside of a call frame")
    let addr = callStack.allocate(type: inst.allocatedType, layout: layout)
    addr.assign(to: .value(inst), in: &registerStack)
    pc!.offset += 1
  }

  private mutating func eval(inst: ApplyInst) {
    let callee = eval(value: inst.fun)
    let args = inst.args.map({ eval(value: $0) })

    // The callee is either a function literal or a reference to a register that holds a thick
    // function. In the former case, `eval(value:)` will have returned a function value. In the
    // latter case, it will have returned a buffer that we should load as a thick function.
    switch callee {
    case let callee as BuiltinFunction:
      let result = callee.apply(to: args, in: &self)
      result.assign(to: .value(inst), in: &registerStack)
      pc!.offset += 1

    case let callee as ThinFunction:
      let function = Unmanaged<Function>.fromOpaque(callee.ptr).takeUnretainedValue()
      invoke(function: function, callerAddr: pc, args: args, returnKey: .value(inst))

    default:
      var thick = callee.open(as: ThickFunction.self)

      // Unroll the chain of delegators, collecting all environment pointers in the process.
      var function: Function?
      var envs: [UnsafeRawBufferPointer] = []
      loop:while true {
        if !thick.env.isEmpty { envs.append(thick.env) }
        switch thick.delegator {
        case .thin(let ptr):
          function = Unmanaged<Function>.fromOpaque(ptr).takeUnretainedValue()
          break loop
        case .thick(let addr):
          withValue(at: addr, boundTo: ThickFunction.self, { thick = $0 })
        }
      }

      // Unpack the environments and invoke the lifted function.

      if envs.isEmpty {
        invoke(function: function!, callerAddr: pc, args: args, returnKey: .value(inst))
        return
      }
    }
  }

  private mutating func eval(inst: LoadInst) {
    let orig = eval(value: inst.lvalue).open(as: ValueAddr.self)
    load(byteCount: layout.size(of: inst.type), from: orig, into: .value(inst))
    pc!.offset += 1
  }

  private mutating func eval(inst: OpenExistentialInst) {
    let container = eval(value: inst.container).open(as: ExistentialContainer.self)

    // Make sure the has the expected witness.
    let witnessPtr = container.witness ?< fatalError("container is empty")
    let witness = Unmanaged<VILType>.fromOpaque(witnessPtr).takeUnretainedValue()
    precondition(witness == inst.type, "bad witness")

    // Load the packaged value, either from the payload of the existential, or from the address
    // stored in the container.
    let pkgSize = layout.size(of: witness)
    if ExistentialContainer.holdsInPayload(
        byteCount: pkgSize, alignedAt: layout.alignment(of: witness))
    {
      withUnsafeBytes(of: container.payload, { (buffer) -> Void in
        let buffer = UnsafeRawBufferPointer(rebasing: buffer[0 ..< pkgSize])
        buffer.assign(to: .value(inst), in: &registerStack)
      })
    } else {
      let buffer = UnsafeRawBufferPointer(
        start: UnsafeRawPointer(bitPattern: container.payload.0), count: container.payload.1)
      buffer.assign(to: .value(inst), in: &registerStack)
    }

    pc!.offset += 1
  }

  private mutating func eval(inst: PartialApplyInst) {
    let fun = eval(value: inst.fun)

    var args: [NewRuntimeValue] = []
    var params: [VILType] = []
    for i in 0 ..< inst.args.count {
      args.append(eval(value: inst.args[i]))
      params.append(inst.args[i].type)
    }

    let env = ThickFunction.makeEnvironment(
      args: inst.args.map({ eval(value: $0) }),
      params: inst.args.map({ $0.type }),
      layout: layout)

    switch fun {
    case is BuiltinFunction:
      env.deallocate()
      fatalError("cannot partially apply built-in function")

    case let fun as ThinFunction:
      let partial = ThickFunction(delegator: .thin(fun.ptr), env: UnsafeRawBufferPointer(env))
      partial.assign(to: .value(inst), in: &registerStack)

    case is ThickFunction:
      fatalError("not implemented")

    default:
      fatalError("'\(fun)' is not a function")
    }

    pc!.offset += 1
  }

  private mutating func eval(inst: RecordMemberInst) {
    let record = eval(value: inst.record) as! UnsafeRawBufferPointer
    let offset = layout.offset(of: inst.memberDecl.name, in: inst.record.type)
      ?< fatalError("failed to compute member offset")

    let byteCount = layout.size(of: inst.type)
    let buffer = UnsafeRawBufferPointer(rebasing: record[offset ..< (offset + byteCount)])
    buffer.assign(to: .value(inst), in: &registerStack)
    pc!.offset += 1
  }

  private mutating func eval(inst: RecordMemberAddrInst) {
    let base = eval(value: inst.record).open(as: ValueAddr.self)
    let offset = layout.offset(of: inst.memberDecl.name, in: inst.record.type.object)
      ?< fatalError("failed to compute member offset")
    base.advanced(by: offset).assign(to: .value(inst), in: &registerStack)
    pc!.offset += 1
  }

  private mutating func eval(inst: RetInst) {
    // First, we need to load the caller's address from the register stack to get the name of the
    // return register in the caller's frame. If there isn't any, then we just have to pop the
    // current frames and we're done.
    guard let callerAddr = registerStack.load(InstAddr?.self, forKey: .callerAddr) else {
      registerStack.popFrame()
      callStack.popFrame()
      pc = nil
      return
    }

    let callerInst = load(instAddr: callerAddr) as? ApplyInst
      ?< fatalError("bad caller address")

    // The return value may be stored in memory that is about to be deallocated from the register
    // stack. We can assume, however, that the return register has been reserved in the previous
    // frame when the function was invoked. Hence, that memory won't be overridden if we only move
    // it to the return register without allocating anything else.
    let value = eval(value: inst.value)

    registerStack.popFrame()
    callStack.popFrame()
    value.assignNoAlloc(to: .value(callerInst), in: &registerStack)

    pc = callerAddr
    pc!.offset += 1
  }

  private mutating func eval(inst: StoreInst) {
    let dest = eval(value: inst.lvalue).open(as: ValueAddr.self)
    store(value: eval(value: inst.rvalue), at: dest)
    pc!.offset += 1
  }

  private mutating func eval(inst: ThinToThickInst) {
    let thick = ThickFunction(thin: functions[inst.ref.name]!)
    thick.assign(to: .value(inst), in: &registerStack)
    pc!.offset += 1
  }

  /// Evaluates a value.
  private mutating func eval(value: Value) -> NewRuntimeValue {
    switch value {
    case is UnitValue:
      return UnsafeRawBufferPointer(start: nil, count: 0)

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
    let function = Unmanaged<Function>.fromOpaque(addr.functionPtr).takeUnretainedValue()
    return function.blocks[addr.blockID]
  }

  /// Dereferences an instruction address.
  private func load(instAddr addr: InstAddr) -> Inst? {
    guard let block = load(blockContaining: addr),
          addr.offset < block.instructions.count
    else { return nil }
    return block.instructions[addr.offset]
  }

  /// Loads `byteCount` bytes from the given address into a register.
  private mutating func load(byteCount: Int, from addr: ValueAddr, into key: RegisterTableKey) {
    switch addr {
    case .stack(let offset):
      callStack.withUnsafeBytes(from: offset, count: byteCount, { buffer in
        registerStack.assign(contentsOf: buffer, to: key)
      })

    case .heap(let ptr):
      registerStack.assign(
        contentsOf: UnsafeRawBufferPointer(start: ptr, count: byteCount), to: key)
    }
  }

  /// Stores a value at the given address.
  private mutating func store(value: NewRuntimeValue, at addr: ValueAddr) {
    switch addr {
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
    case .stack(let offset):
      return callStack.withUnsafeMutableRawPointer(from: offset, { ptr in
        body(&ptr.assumingMemoryBound(to: T.self).pointee)
      })
    case .heap(let ptr):
      return body(&ptr.assumingMemoryBound(to: T.self).pointee)
    }
  }

}
