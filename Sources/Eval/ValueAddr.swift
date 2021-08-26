/// The address of a runtime value.
public enum ValueAddr {

  /// A stack address, represented as an offset from the stack's base address.
  case stack(Int)

  /// A heap address.
  case heap(UnsafeMutableRawPointer)

  /// Returns an address offset by the specified value.
  ///
  /// - Parameter offset: The offset to apply to this address.
  func advanced(by offset: Int) -> ValueAddr {
    switch self {
    case .stack(let i):
      return .stack(i + offset)
    case .heap(let ptr):
      return .heap(ptr.advanced(by: offset))
    }
  }

}
