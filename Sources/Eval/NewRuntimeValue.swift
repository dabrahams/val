import VIL

/// A runtime value.
protocol NewRuntimeValue {

  var _byteCount: Int { get }

  func open<T>(as: T.Type) -> T

  func store(to pointer: UnsafeMutableRawPointer)

  func assign(to key: RegisterTableKey, in stack: inout RegisterStack)

  func assignNoAlloc(to key: RegisterTableKey, in stack: inout RegisterStack)

}

extension NewRuntimeValue {

  var _byteCount: Int { MemoryLayout<Self>.size }

  func open<T>(as: T.Type) -> T {
    return self as! T
  }

  func open(as: UnsafeRawBufferPointer) -> UnsafeRawBufferPointer {
    return self as! UnsafeRawBufferPointer
  }

  func assign(to key: RegisterTableKey, in stack: inout RegisterStack) {
    stack.assign(value: self, to: key)
  }

  func assignNoAlloc(to key: RegisterTableKey, in stack: inout RegisterStack) {
    withUnsafeBytes(of: self, { buffer in
      stack.assignNoAlloc(contentsOf: buffer, to: key)
    })
  }

  func store(to pointer: UnsafeMutableRawPointer) {
    pointer.assumingMemoryBound(to: Self.self).pointee = self
  }

}

extension BuiltinFunction: NewRuntimeValue {}

extension ExistentialContainer: NewRuntimeValue {}

extension Int: NewRuntimeValue {}

extension InstAddr: NewRuntimeValue {}

extension Optional: NewRuntimeValue where Wrapped == InstAddr {}

extension ValueAddr: NewRuntimeValue {}

extension ThickFunction: NewRuntimeValue {}

extension ThinFunction: NewRuntimeValue {}

extension UnsafeRawBufferPointer: NewRuntimeValue {

  var _byteCount: Int { count }

  func open<T>(as: T.Type) -> T {
    load(as: T.self)
  }

  func store(to pointer: UnsafeMutableRawPointer) {
    pointer.copyMemory(from: baseAddress!, byteCount: count)
  }

  func assign(to key: RegisterTableKey, in stack: inout RegisterStack) {
    stack.assign(contentsOf: self, to: key)
  }

  func assignNoAlloc(to key: RegisterTableKey, in stack: inout RegisterStack) {
    stack.assignNoAlloc(contentsOf: self, to: key)
  }

}
