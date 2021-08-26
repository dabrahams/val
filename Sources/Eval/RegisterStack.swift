import Basic
import VIL

/// A table mapping register keys to a value.
typealias RegisterTable = [RegisterTableKey: RegisterEntryInfo]

/// A key in a register table.
enum RegisterTableKey: Hashable {

  /// A key identifying the caller address register.
  case callerAddr

  /// A key identifying a value register.
  case value(Value)

  /// Encodes the key into a raw value, using pointer tagging to represent the `callerAddr` case.
  var rawValue: Int {
    switch self {
    case .callerAddr:
      return 1
    case .value(let inst):
      return Int(bitPattern: ObjectIdentifier(inst))
    }
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(rawValue)
  }

  static func == (lhs: RegisterTableKey, rhs: RegisterTableKey) -> Bool {
    return lhs.rawValue == rhs.rawValue
  }

}

/// A data structure providing information about a specific entry in a register table.
struct RegisterEntryInfo {

  private var rawValue: UInt64

  init(offset: Int, count: Int, busy: Bool) {
    assert(offset >> 48 == 0)
    assert(count >> 15 == 0)
    rawValue = UInt64(truncatingIfNeeded: offset) | UInt64(truncatingIfNeeded: count) << 48
    if busy { rawValue |= (1 << 63) }
  }

  var offset: Int {
    get { Int(truncatingIfNeeded: rawValue & (1 << 48 - 1)) }
    set { rawValue = rawValue & ~(1 << 48 - 1) | UInt64(truncatingIfNeeded: newValue) }
  }

  var count: Int {
    get { Int(truncatingIfNeeded: (rawValue >> 48) & ~(1 << 15)) }
    set { rawValue = rawValue & (~0 >> 16 | 1 << 63) | UInt64(truncatingIfNeeded: newValue) << 48 }
  }

  var busy: Bool {
    get { Int(truncatingIfNeeded: rawValue >> 63) != 0 }
    set { rawValue = rawValue & (~0 >> 1) | (newValue ? (1 << 63) : 0) }
  }

}

/// A register stack.
///
/// Each frame is a table of "virtual registers", mapping register keys to their values.
struct RegisterStack {

  /// A frame in a register stack.
  struct Frame {

    fileprivate typealias Header = (previousFrameOffset: Int, table: RegisterTable)

    /// A pointer to the stack containing this frame.
    fileprivate var stack: UnsafePointer<RegisterStack>

    /// The offset of this frame from the stack's base address.
    let start: Int

    fileprivate init(stack: UnsafePointer<RegisterStack>, start: Int) {
      self.stack = stack
      self.start = start
    }

    /// The frame offset of the previous frame.
    var previousFrameOffset: Int? {
      let i = stack.pointee.memory.baseAddress!
        .advanced(by: start)
        .assumingMemoryBound(to: Header.self)
        .pointee.previousFrameOffset
      return i != -1 ? i : nil
    }

    /// The keys of the virtual register table in this frame.
    var keys: RegisterTable.Keys {
      let header = stack.pointee.memory.baseAddress!
        .advanced(by: start)
        .assumingMemoryBound(to: Frame.Header.self)
      return header.pointee.table.keys
    }

    /// Loads the contents of the given register.
    subscript<T>(_ key: RegisterTableKey, as _: T.Type) -> T {
      let header = stack.pointee.memory.baseAddress!
        .advanced(by: start)
        .assumingMemoryBound(to: Frame.Header.self)
      guard let i = header.pointee.table[key] else { fatalError("no value for the given key") }

      return stack.pointee.memory.baseAddress!
        .load(fromByteOffset: i.offset, as: T.self)
    }

  }

  /// The memory of the stack.
  fileprivate var memory: UnsafeMutableRawBufferPointer

  /// A pointer to the top of the stack.
  fileprivate var top: Int

  /// The offset of the last frame from the stack's base address.
  fileprivate var lastFrameOffset: Int

  /// Creates a call stack.
  ///
  /// - Parameter initialCapacity: The initial capacity of the stack, in bytes.
  init(initialCapacity: Int) {
    let byteCount = max(initialCapacity, MemoryLayout<Frame.Header>.size)
    memory = .allocate(byteCount: byteCount, alignment: Self.defaultAlignment)
    top = 0
    lastFrameOffset = -1
  }

  mutating func deinitialize() {
    while lastFrameOffset != -1 { popFrame() }
    memory.deallocate()
  }

  /// A Boolean value indicating whether the stack is empty.
  var isEmpty: Bool {
    return lastFrameOffset == -1
  }

  /// Pushes a new frame onto the stack.
  mutating func pushFrame() {
    let base = nextOffset(alignedAt: MemoryLayout<Frame.Header>.alignment, from: top)
    precondition(base + MemoryLayout<Frame.Header>.size < memory.count, "stack overflow")

    memory.baseAddress!
      .advanced(by: base)
      .initializeMemory(
        as: Frame.Header.self,
        repeating: (previousFrameOffset: lastFrameOffset, table: [:]),
        count: 1)
    lastFrameOffset = base
    top = base + MemoryLayout<Frame.Header>.size
  }

  /// Pops a frame from the stack, deinitializing its header.
  mutating func popFrame() {
    precondition(!isEmpty, "the stack is empty")

    // Restore the frame index and deinitializes the frame.
    let header = memory.baseAddress!
      .advanced(by: lastFrameOffset)
      .assumingMemoryBound(to: Frame.Header.self)
    top = lastFrameOffset
    lastFrameOffset = header.pointee.previousFrameOffset
    header.deinitialize(count: 1)
  }

  mutating func assign<T>(value: T, to key: RegisterTableKey) where T: NewRuntimeValue {
    assert(isTrivial(T.self))
    let ptr = allocate(
      byteCount: MemoryLayout<T>.size,
      alignedAt: MemoryLayout<T>.alignment,
      busy: true,
      forKey: key)
    ptr.initializeMemory(as: T.self, repeating: value, count: 1)
  }

  mutating func assign(
    contentsOf buffer: UnsafeRawBufferPointer,
    alignedAt alignment: Int = defaultAlignment,
    to key: RegisterTableKey
  ) {
    let ptr = allocate(
      byteCount: buffer.count,
      alignedAt: alignment,
      busy: true,
      forKey: key)

    if buffer.count > 0 {
      ptr.copyMemory(from: buffer.baseAddress!, byteCount: buffer.count)
    }
  }

  mutating func assignNoAlloc(
    contentsOf buffer: UnsafeRawBufferPointer, to key: RegisterTableKey
  ) {
    precondition(!isEmpty, "the stack is empty")

    let header = memory.baseAddress!
      .advanced(by: lastFrameOffset)
      .assumingMemoryBound(to: Frame.Header.self)
    let info = header.pointee.table[key] ?< fatalError("register is not reserved")
    precondition(!info.busy, "register is assigned")
    precondition(info.count >= buffer.count, "buffer is too large")

    if buffer.count > 0 {
      let ptr = memory.baseAddress!.advanced(by: info.offset)
      ptr.copyMemory(from: buffer.baseAddress!, byteCount: buffer.count)
    }
    header.pointee.table[key]!.busy = true
  }

  mutating func reserve(
    registerOfType type: VILType, layout: DataLayout, forKey key: RegisterTableKey
  ) {
    _ = allocate(
      byteCount: layout.size(of: type),
      alignedAt: layout.alignment(of: type),
      busy: false,
      forKey: key)
  }

  private mutating func allocate(
    byteCount: Int,
    alignedAt alignment: Int,
    busy: Bool,
    forKey key: RegisterTableKey
  ) -> UnsafeMutableRawPointer {
    precondition(!isEmpty, "the stack is empty")

    // Align the base address of the new allocation.
    let base = nextOffset(alignedAt: alignment, from: top)
    precondition(base + byteCount < memory.count, "stack overflow")

    let header = memory.baseAddress!
      .advanced(by: lastFrameOffset)
      .assumingMemoryBound(to: Frame.Header.self)
    precondition(header.pointee.table[key] == nil, "register is reserved")

    // Zero-initialize the allocated memory.
    let ptr = memory.baseAddress!.advanced(by: base)
    ptr.initializeMemory(as: UInt8.self, repeating: 0, count: byteCount)

    header.pointee.table[key] = RegisterEntryInfo(offset: base, count: byteCount, busy: busy)
    top = base + byteCount
    return ptr
  }

  func load<T>(_: T.Type, forKey key: RegisterTableKey) -> T {
    return unsafeRawBufferPointer(forKey: key).load(as: T.self)
  }

  /// Returns a buffer with the contents of the register for the given key.
  ///
  /// - Warning: The returned buffer is created over the internal memory of this register stack.
  ///   That means that the contents of that buffer will be invalidated by any mutating operation
  ///   on the register stack.
  ///
  /// - Parameter key: A register key.
  func unsafeRawBufferPointer(forKey key: RegisterTableKey) -> UnsafeRawBufferPointer {
    precondition(!isEmpty, "the stack is empty")

    let header = memory.baseAddress!
      .advanced(by: lastFrameOffset)
      .assumingMemoryBound(to: Frame.Header.self)
    guard let i = header.pointee.table[key] else { fatalError("no value for the given key") }

    return UnsafeRawBufferPointer(rebasing: memory[i.offset ..< i.offset + i.count])
  }

  /// Calls the given closure with a reference to the frame identified by the given index.
  ///
  /// This method is useful to access the contents of a frame directly. The argument passed to the
  /// closure is an adapter that abstracts over low-level memory management operations.
  ///
  /// The argument to `body` is valid only during the execution of `withFrame(at:_:)`. Do not store
  /// or return that value for later use.
  ///
  /// - Parameters:
  ///   - frameIndex: A frame index. The last (i.e. most recent) frame is indexed by 0.
  ///   - body: A closure that accepts a reference to a frame.
  func withFrame<R>(at frameIndex: Int, _ body: (Frame) -> R) -> R {
    precondition(!isEmpty, "the stack is empty")

    let offset = frameOffset(of: frameIndex)
    return withUnsafePointer(to: self, { stack in
      return body(Frame(stack: stack, start: offset))
    })
  }

  /// Calls the given closure on each frame of the stack.
  ///
  /// The argument to `body` is valid only during the execution of `forEach(_:)`. Do not store that
  /// that value for later use.
  ///
  /// - Parameter body: A closure that accepts a reference to a frame and returns whether it should
  ///   be called with the next frame.
  /// - Returns: `true` if `body` returned `true` for every frame; otherwise, `false`.
  @discardableResult
  func forEach(_ body: (Frame) throws -> Bool) rethrows -> Bool {
    var offset = lastFrameOffset
    while offset != -1 {
      let c = try withUnsafePointer(to: self, { stack in
        return try body(Frame(stack: stack, start: offset))
      })
      guard c else { return false }

      offset = memory.baseAddress!
        .advanced(by: offset)
        .assumingMemoryBound(to: Frame.Header.self)
        .pointee.previousFrameOffset
    }

    return true
  }

  private func frameOffset(of frameIndex: Int) -> Int {
    var offset = lastFrameOffset
    var index = frameIndex

    while index > 0 {
      offset = memory.baseAddress!
        .advanced(by: offset)
        .assumingMemoryBound(to: Frame.Header.self)
        .pointee.previousFrameOffset
      precondition(offset != -1, "index is out of bound")
      index -= 1
    }
    return offset
  }

  private static let defaultAlignment = MemoryLayout<Int>.alignment

}

// MARK: CustomReflectable

extension RegisterEntryInfo: CustomReflectable {

  var customMirror: Mirror {
    return Mirror(self, children: ["offset": offset, "count": count, "busy": busy])
  }

}
