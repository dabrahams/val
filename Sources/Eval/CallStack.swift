import VIL

/// A call stack.
///
/// Memory is 8 bytes aligned, assuming the native machine has 64-bit integers.
struct CallStack {

  fileprivate typealias RTTI = [(offset: Int, type: VILType)]

  fileprivate typealias FrameHeader = (previousFrameOffset: Int, rtti: RTTI)

  struct FrameBuffer {

    fileprivate let buffer: UnsafeMutableRawBufferPointer

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
    let byteCount = max(initialCapacity, MemoryLayout<FrameHeader>.size)
    memory = .allocate(byteCount: byteCount, alignment: Self.defaultAlignment)
    top = 0
    lastFrameOffset = -1
  }

  mutating func deinitialize() {
    while lastFrameOffset != -1 { removeFrame() }
    memory.deallocate()
  }

  /// A Boolean value indicating whether the stack is empty.
  var isEmpty: Bool {
    return lastFrameOffset == -1
  }

  /// Pushes a frame onto the stack.
  mutating func pushFrame() {
    let base = nextOffset(alignedAt: MemoryLayout<FrameHeader>.alignment, from: top)
    if base + MemoryLayout<FrameHeader>.size > memory.count {
      expand(minimumCapacity: base + MemoryLayout<FrameHeader>.size)
    }

    memory.baseAddress!
      .advanced(by: base)
      .initializeMemory(
        as: FrameHeader.self,
        repeating: (previousFrameOffset: lastFrameOffset, rtti: []),
        count: 1)
    lastFrameOffset = base
    top = base + MemoryLayout<FrameHeader>.size
  }

  /// Removes the latest frame from the stack, deinitializing its header.
  mutating func removeFrame() {
    precondition(!isEmpty, "the stack is empty")

    // Restore the offset of the previous frame and deinitializes the current one.
    let header = memory.baseAddress!
      .advanced(by: lastFrameOffset)
      .assumingMemoryBound(to: FrameHeader.self)
    top = lastFrameOffset
    lastFrameOffset = header.pointee.previousFrameOffset
    header.deinitialize(count: 1)
  }

  /// Allocates new space at the top of the stack.
  mutating func allocate(type: VILType, layout: DataLayout) -> ValueAddr {
    precondition(!isEmpty, "the stack is empty")

    let base = nextOffset(alignedAt: layout.alignment(of: type), from: top)
    let byteCount = layout.size(of: type)
    if base + byteCount >= memory.count {
      expand(minimumCapacity: base + byteCount)
    }

    // Store the RTTI.
    let header = memory.baseAddress!
      .advanced(by: lastFrameOffset)
      .assumingMemoryBound(to: FrameHeader.self)
    header.pointee.rtti.append((offset: base - lastFrameOffset, type: type))

    // Zero-initialize the allocated memory.
    memory.baseAddress!.advanced(by: base)
      .initializeMemory(as: UInt8.self, repeating: 0, count: byteCount)

    let addr = ValueAddr.stack(base)
    top = base + byteCount
    return addr
  }

  /// Deallocates a value from the top of the stack. The memory must be deinitialized.
  mutating func pop(byteCount: Int, as type: VILType) {
    precondition(!isEmpty, "the stack is empty")

    let header = memory.baseAddress!
      .advanced(by: lastFrameOffset)
      .assumingMemoryBound(to: FrameHeader.self)

    guard let last = header.pointee.rtti.last,
          last.type == type
    else { fatalError("top of the stack does not have the expected type") }

    header.pointee.rtti.removeLast()
    top -= byteCount
  }

  /// Copies `count` bytes of from the location pointed to by `src` directly to the memory pointed
  /// to by `dst`.
  mutating func copyMemory(to dst: Int, from src: Int, count: Int) {
    precondition(dst + count <= top, "destination address is out of range")
    precondition(src + count <= top, "source address is out of range")
    memory.baseAddress!
      .advanced(by: dst)
      .copyMemory(from: memory.baseAddress!.advanced(by: src), byteCount: count)
  }

  /// Calls the given closure with a pointer referencing the memory at the given offset.
  ///
  /// - Note: The method is mutating to guarantee that it has exclusive access to the stack's
  ///   memory while `body` is executed.
  ///
  /// - Parameters:
  ///   - offset: An offset from the base address of this stack.
  ///   - body: A closure that takes a pointer referencing the memory at the given offset.
  ///     The argument is valid only for the duration of the closure's execution.
  mutating func withUnsafeRawPointer<R>(
    from offset: Int, _ body: (UnsafeRawPointer) -> R
  ) -> R {
    precondition(offset <= top, "address is out of range")
    return body(UnsafeRawPointer(memory.baseAddress!.advanced(by: offset)))
  }

  /// Calls the given closure with a mutable pointer referencing the memory at the given offset.
  ///
  /// - Parameters:
  ///   - offset: An offset from the base address of this stack.
  ///   - body: A closure that takes a mutable pointer referencing the memory at the given offset.
  ///     The argument is valid only for the duration of the closure's execution.
  mutating func withUnsafeMutableRawPointer<R>(
    from offset: Int, _ body: (UnsafeMutableRawPointer) -> R
  ) -> R {
    precondition(offset < top, "address is out of range")
    return body(memory.baseAddress!.advanced(by: offset))
  }

  /// Expands the size of the stack.
  private mutating func expand(minimumCapacity: Int) {
    let byteCount = max(minimumCapacity, memory.count * 2)
    let newMemory = UnsafeMutableRawBufferPointer.allocate(byteCount: byteCount, alignment: 8)
    newMemory.copyBytes(from: memory)
    memory.deallocate()
    memory = newMemory
  }

  private static let defaultAlignment = MemoryLayout<Int>.alignment

}

// MARK: CustomReflectable

extension CallStack: CustomReflectable {

  var customMirror: Mirror {
    var children: [Mirror.Child] = []

    var start = lastFrameOffset
    var end = top
    while start != -1 {
      let frame = CallFrameReflection(memory: memory, start: start, byteCount: end - start)
      children.append((label: "+\(start)", value: frame))
      end = start
      start = frame.previousFrameOffset
    }

    return Mirror(self, children: children, displayStyle: .collection)
  }

}

fileprivate struct CallFrameReflection: CustomReflectable {

  let previousFrameOffset: Int

  let children: [(label: String, value: CallFrameCellReflection)]

  init(memory: UnsafeMutableRawBufferPointer, start: Int, byteCount: Int) {
    let header = memory.baseAddress!
      .advanced(by: start)
      .load(as: CallStack.FrameHeader.self)

    var children: [(String, CallFrameCellReflection)] = []
    for i in 0 ..< header.rtti.count {
      let (offset, type) = header.rtti[i]
      let upper = i < (header.rtti.count - 1) ? header.rtti[i + 1].offset : byteCount
      let slice = memory[(start + offset) ..< (start + upper)]
      let value = CallFrameCellReflection(
        contents: UnsafeRawBufferPointer(rebasing: slice), type: type)

      children.append((label: "+\(start + offset)", value: value))
    }

    self.previousFrameOffset = header.previousFrameOffset
    self.children = children.reversed()
  }

  var customMirror: Mirror {
    return Mirror(self, children: children, displayStyle: .collection)
  }

}

fileprivate struct CallFrameCellReflection: CustomReflectable {

  let contents: UnsafeRawBufferPointer

  let type: VILType

  var customMirror: Mirror {
    var buf = contents.prefix(8)
      .map({ (byte) -> String in
        let s = String(byte, radix: 16)
        return byte < 16 ? "0\(s)" : s
      })
      .joined(separator: " ")
    if contents.count > 8 { buf += "... and \(contents.count - 8) more bytes" }
    return Mirror(self, children: ["type": type, "contents": buf])
  }

}
