import Utils
import FrontEnd 

/// A result builder that helps construct IR by chaining instructions.
@resultBuilder
public struct IRBuilder {
  // ...existing code...
}

/// A builder instruction representing an IR operation to be performed.
public protocol BuilderInstruction {
  /// Performs the instruction using the given module.
  func build(in module: inout Module, at point: InsertionPoint) -> (Operand?, InsertionPoint)
}

/// An IR sequence to be inserted at a specific location
public struct IRSequence {
  let instructions: [BuilderInstruction]
  let site: SourceRange
  
  init(_ instructions: [BuilderInstruction], at site: SourceRange) {
    self.instructions = instructions
    self.site = site
  }
}

/// A builder context that maintains state during IR construction.
public struct BuilderContext {
  /// The module being modified.
  public var module: Module
  
  /// The current insertion point.
  public var insertionPoint: InsertionPoint

  /// Creates a new builder context.
  public init(module: Module, insertionPoint: InsertionPoint) {
    self.module = module
    self.insertionPoint = insertionPoint
  }

  /// Builds a sequence of instructions with a common site.
  public mutating func build(
    at site: SourceRange,
    @IRBuilder _ content: () -> [BuilderInstruction]
  ) -> Operand? {
    var result: Operand?
    for instruction in content() {
      (result, insertionPoint) = instruction.build(in: &module, at: insertionPoint)
    }
    return result
  }
}

/// Helper functions to create builder instructions without explicit sites
extension BuilderInstruction {
  /// Sets the site from the containing sequence
  func withSite(_ site: SourceRange) -> BuilderInstruction {
    // Default implementation just returns self
    // Individual instruction types should override
    self
  }
}
