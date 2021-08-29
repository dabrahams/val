import VIL

/// A runtime value.
protocol RuntimeValue {

  var _byteCount: Int { get }

  func open<T>(as: T.Type) -> T

  func store(to pointer: UnsafeMutableRawPointer)

  func assign(to key: RegisterTableKey, in stack: inout RegisterStack)

  func assignNoAlloc(to key: RegisterTableKey, in stack: inout RegisterStack)

  func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) -> R) -> R

}

extension RuntimeValue {

  var _byteCount: Int { MemoryLayout<Self>.size }

  func open<T>(as: T.Type) -> T {
    return self as! T
  }

  func open(as: UnsafeRawBufferPointer) -> UnsafeRawBufferPointer {
    return self as! UnsafeRawBufferPointer
  }

  func store(to pointer: UnsafeMutableRawPointer) {
    pointer.assumingMemoryBound(to: Self.self).pointee = self
  }

  func assign(to key: RegisterTableKey, in stack: inout RegisterStack) {
    stack.assign(value: self, to: key)
  }

  func assignNoAlloc(to key: RegisterTableKey, in stack: inout RegisterStack) {
    Swift.withUnsafeBytes(of: self, { buffer in
      stack.assignNoAlloc(contentsOf: buffer, to: key)
    })
  }

  func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) -> R) -> R {
    return Swift.withUnsafeBytes(of: self, body)
  }

}

extension Bool: RuntimeValue {}

extension BuiltinFunction: RuntimeValue {}

extension ExistentialContainer: RuntimeValue {}

extension InstAddr: RuntimeValue {}

extension Int: RuntimeValue {}

extension ValueAddr: RuntimeValue {}

extension ThickFunction: RuntimeValue {}

extension ThinFunction: RuntimeValue {}

extension UnsafeRawBufferPointer: RuntimeValue {

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

  func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) -> R) -> R {
    return body(self)
  }

}
