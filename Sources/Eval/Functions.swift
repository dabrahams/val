import class Foundation.FileHandle
import VIL

/// A "thin" function, i.e., a single function pointer.
struct ThinFunction {

  /// A function pointer.
  let ptr: UnsafeRawPointer

  init(function: VIL.Function) {
    ptr = UnsafeRawPointer(Unmanaged.passUnretained(function).toOpaque())
  }

}

/// A "thick" function, pairing a (potentially thick) function with a pointer to its environment.
struct ThickFunction {

  /// A value representing the delegator of a partially applied function.
  enum Delegator {

    case thin(UnsafeRawPointer)

    case thick(ValueAddr)

  }

  /// The function to which the call is forwarded.
  let delegator: Delegator

  /// A buffer representing the function's environment.
  ///
  /// The buffer starts with a pair `(count, align)` of 32-bit unsigned integers denoting the
  /// number of captures and their memory alignment `align`, respectively. After that header, there
  /// is a sequence of `count` pointers to the value witness table of each capture. The captures
  /// are laid out after these pointers, represented as an instance of a compound type.
  let env: UnsafeRawBufferPointer

  init(delegator: Delegator, env: UnsafeRawBufferPointer) {
    self.delegator = delegator
    self.env = env
  }

  init(thin function: VIL.Function) {
    self.delegator = .thin(UnsafeRawPointer(Unmanaged.passUnretained(function).toOpaque()))
    self.env = UnsafeRawBufferPointer(start: nil, count: 0)
  }

  func unpackEnvironment(layout: DataLayout) -> [NewRuntimeValue] {
    guard !env.isEmpty else { return [] }

    // Compute the properties of the environment's memory layout.
    let count = Int(truncatingIfNeeded: env.load(fromByteOffset: 0, as: UInt32.self))
    let align = Int(truncatingIfNeeded: env.load(fromByteOffset: 4, as: UInt32.self))
    let headerByteCount = 8 + MemoryLayout<UnsafeRawPointer>.stride * count
    let witnessTables = env.baseAddress!
      .advanced(by: 8)
      .assumingMemoryBound(to: UnsafeRawPointer.self)

    // Unpack the payload.
    var offset = nextOffset(alignedAt: align, from: headerByteCount)
    var values: [NewRuntimeValue] = []
    for i in 0 ..< count {
      let vwt = Unmanaged<VILType>.fromOpaque(witnessTables[i]).takeUnretainedValue()
      let byteCount = layout.size(of: vwt)
      values.append(UnsafeRawBufferPointer(rebasing: env[offset ..< offset + byteCount]))
      offset += nextOffset(alignedAt: align, from: offset + byteCount)
    }

    return values
  }

  static func makeEnvironment(
    args: [NewRuntimeValue], params: [VILType], layout: DataLayout
  ) -> UnsafeRawBufferPointer {
    // Compute the properties of the environment's memory layout.
    let payloadOffsets = layout.offsets(in: params)
    let align = params.reduce(8, { max($0, layout.alignment(of: $1)) })
    let headerByteCount = 8 + MemoryLayout<UnsafeRawPointer>.stride * params.count
    let offset = nextOffset(alignedAt: align, from: headerByteCount)

    // Allocate the environment buffer.
    let env = UnsafeMutableRawBufferPointer.allocate(
      byteCount: offset + payloadOffsets.last!, alignment: align)
    let witnessTables = env.baseAddress!
      .advanced(by: 8)
      .assumingMemoryBound(to: UnsafeRawPointer.self)

    // Pack the arguments into the environment.
    for i in 0 ..< params.count {
      witnessTables[i] = UnsafeRawPointer(Unmanaged.passUnretained(params[i]).toOpaque())
      args[i].store(to: env.baseAddress!.advanced(by: offset + payloadOffsets[i]))
    }

    return UnsafeRawBufferPointer(env)
  }

}

/// A built-in function.
struct BuiltinFunction {

  enum ID: String {

    case set_status

    case i64_print
    case i64_trunc_IntLiteral
    case i64_add
    case i64_sub
    case i64_mul
    case i64_div
    case i64_rem
    case i64_neg
    case i64_abs

  }

  var id: ID

  init(id: ID) {
    self.id = id
  }

  init?(literal: BuiltinFunRef) {
    guard let id = ID(rawValue: literal.decl.name) else { return nil }
    self.init(id: id)
  }

  func apply(to args: [NewRuntimeValue], in interpreter: inout NewInterpreter) -> NewRuntimeValue {
    switch id {
    case .set_status:
      let a = args[0].open(as: Int.self)
      interpreter.status = a
      return UnsafeRawBufferPointer(start: nil, count: 0)

    case .i64_print:
      let a = args[0].open(as: Int.self)
      interpreter.standardOutput.write("\(a)\n".data(using: .utf8)!)
      return UnsafeRawBufferPointer(start: nil, count: 0)

    case .i64_trunc_IntLiteral:
      assert(args[0]._byteCount == MemoryLayout<Int>.size)
      return args[0]

    case .i64_add:
      let a = args[0].open(as: Int.self)
      let b = args[1].open(as: Int.self)
      return a + b

    case .i64_sub:
      let a = args[0].open(as: Int.self)
      let b = args[1].open(as: Int.self)
      return a - b

    case .i64_mul:
      let a = args[0].open(as: Int.self)
      let b = args[1].open(as: Int.self)
      return a * b

    case .i64_div:
      let a = args[0].open(as: Int.self)
      let b = args[1].open(as: Int.self)
      return a / b

    case .i64_rem:
      let a = args[0].open(as: Int.self)
      let b = args[1].open(as: Int.self)
      return a % b

    case .i64_neg:
      let a = args[0].open(as: Int.self)
      return -a

    case .i64_abs:
      let a = args[0].open(as: Int.self)
      return abs(a)
    }
  }

}
