/// An object that can consume and report in-flight diagnostics.
public protocol DiagConsumer {

  /// Consumes and reports a diagnostic.
  ///
  /// - Parameter diagnostic: A diagnostic.
  mutating func consume(_ diagnostic: Diag)

}

/// An in-flight diagnostic about a compilation issue.
public struct Diag {

  /// Creates an in-flight diagnostic.
  ///
  /// - Parameters:
  ///   - level: The severity of the diagnostic.
  ///   - message: The message of the diagnostic.
  ///   - anchor: A source range related to the diagnostic. If assigned, the diagnostic is anchored
  ///     at the range's start location.
  public init(_ level: Level, _ message: String, anchor: SourceRange? = nil) {
    self.message = message
    self.level = level

    if let range = anchor {
      reportLocation = range.lowerBound
      ranges = [range]
    }
  }

  /// Creates an in-flight error diagnostic.
  ///
  /// - Parameters:
  ///   - message: The message of the diagnostic.
  ///   - anchor: A source range related to the diagnostic. If assigned, the diagnostic is anchored
  ///     at the range's start location.
  public init(_ message: String, anchor: SourceRange? = nil) {
    self.init(.error, message, anchor: anchor)
  }

  /// The message of the diagnostic.
  public let message: String

  /// The level of the diagnostic.
  public let level: Level

  /// The location at which the diagnostic should be reported.
  public var reportLocation: SourceRange.Bound?

  /// The source ranges related to this diagnostic.
  public var ranges: [SourceRange] = []

  public func set<T>(_ key: WritableKeyPath<Diag, T>, value: T) -> Diag {
    var copy = self
    copy[keyPath: key] = value
    return copy
  }

  /// The severity of a diagnostic.
  public enum Level: Int, Comparable {

    /// An error that does not prevent compilation.
    case warning = 0

    /// An unrecoverable error that prevents compilation.
    case error

    public static func < (lhs: Diag.Level, rhs: Diag.Level) -> Bool {
      lhs.rawValue < rhs.rawValue
    }

  }

}

extension Diag.Level: CustomStringConvertible {

  public var description: String {
    switch self {
    case .error   : return "error"
    case .warning : return "warning"
    }
  }

}
