import Foundation

/// A collection of top-level declarations, abstracting over the concept of a "file".
///
/// Each file unit delimits a declaration space, which serves as a boundary for the visibility of
/// imported symbols and file protected declarations. Howrver, its does not function the same way
/// as other kinds of declaration spaces with respect to name lookup. Top-level entities declared
/// in a unit are merged with those from all of its siblings and should be looked up from the
/// enclosing module.
public class FileUnit: DeclSpace {

  /// The module in which the unit resides.
  public var parentDeclSpace: DeclSpace?

  /// The top-level declarations of the unit.
  public var decls: [Decl] = []

  /// The URL of the source file containing this unit's contents, or nil indicating this is the
  /// builtin module.
  public let url: URL?

  public init(url: URL?) {
    self.url = url
  }
  
  /// Overrides the default lookup mechanism.
  public func lookup(qualified name: String) -> LookupResult {
    return LookupResult()
  }

}
