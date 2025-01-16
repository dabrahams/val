import FrontEnd
import Utils

/// Helper functions to create builder instructions
extension BuilderContext {

  func alloc(_ type: AnyType, at site: SourceRange) -> AllocStack {
    AllocStack(type: type, site: site) 
  }

  func access(
    _ effect: AccessEffect,
    operand: Operand,
    at site: SourceRange
  ) -> Access {
    Access(effect: effect, operand: operand, site: site)
  }

  func load(_ operand: Operand, at site: SourceRange) -> Load {
    Load(operand: operand, site: site) 
  }

  func store(_ value: Operand, at target: Operand, at site: SourceRange) -> Store {
    Store(value: value, target: target, site: site)
  }

  // Add more helpers as needed...
}
