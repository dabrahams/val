import Basic

/// A pattern.
public protocol Pattern: Node {

  /// The type of the pattern.
  var type: ValType { get set }

  /// Returns the named patterns contained within this pattern.
  var namedPatterns: [NamedPattern] { get }

}

/// A pattern which binds an identifier.
public final class NamedPattern: Pattern {

  public init(decl: VarDecl, range: SourceRange) {
    self.decl = decl
    self.range = range
  }

  /// The variable declarations to which the name refers.
  public var decl: VarDecl

  public var type: ValType {
    get { decl.type }
    set { precondition(newValue === decl.type) }
  }

  public var namedPatterns: [NamedPattern] { [self] }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// A comma-separated list of zero or more patterns, enclosed in parentheses.
public final class TuplePattern: Pattern {

  public init(elems: [Elem], type: ValType, range: SourceRange) {
    self.elems = elems
    self.type = type
    self.range = range
  }

  /// The elements of the tuple.
  public var elems: [Elem]

  public var type: ValType

  public var namedPatterns: [NamedPattern] {
    return Array(elems.map({ el in el.pattern.namedPatterns }).joined())
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

  /// An element in a tuple pattern.
  public struct Elem {

    public init(label: String?, pattern: Pattern, range: SourceRange) {
      self.label = label
      self.pattern = pattern
      self.range = range
    }

    /// The label of the element.
    public var label: String?

    /// The pattern of the element.
    public var pattern: Pattern

    /// The source range of this element's textual representation.
    public var range: SourceRange

  }

}

/// A pattern that matches an arbitrary value, but does not bind it to a name.
public final class WildcardPattern: Pattern {

  public init(type: ValType, range: SourceRange) {
    self.type = type
    self.range = range
  }

  public var type: ValType

  public var namedPatterns: [NamedPattern] { [] }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}