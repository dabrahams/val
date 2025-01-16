import FrontEnd
import Utils

/// Helper functions to create builder instructions
extension BuilderContext {

  func alloc(_ type: AnyType, at site: SourceRange) -> AllocStack {
    AllocStack(type: type, site: site) 
  }

  func alloc(_ type: AnyType) -> AllocStack {
    AllocStack(type: type) 
  }

  func access(
    _ effect: AccessEffect,
    operand: Operand,
    at site: SourceRange
  ) -> Access {
    Access(effect: effect, operand: operand, site: site)
  }

  func access(_ effect: AccessEffect, operand: Operand) -> Access {
    Access(effect: effect, operand: operand)
  }

  func load(_ operand: Operand, at site: SourceRange) -> Load {
    Load(operand: operand, site: site) 
  }

  func load(_ operand: Operand) -> Load {
    Load(operand: operand)
  }

  func store(_ value: Operand, at target: Operand, at site: SourceRange) -> Store {
    Store(value: value, target: target, site: site)
  }

  func store(_ value: Operand, at target: Operand) -> Store {
    Store(value: value, target: target)
  }

  // Add more helpers as needed...
}

// Individual instruction implementations:
struct AllocStack: BuilderInstruction {
  let type: AnyType
  var site: SourceRange?
  
  init(type: AnyType, site: SourceRange? = nil) {
    self.type = type
    this.site = site
  }
  
  func withSite(_ site: SourceRange) -> BuilderInstruction {
    var copy = self
    copy.site = site
    return copy
  }
  
  func build(in module: inout Module, at point: InsertionPoint) -> (Operand?, InsertionPoint) {
    let i = module.insert(
      module.makeAllocStack(type, at: site ?? point.block.site),
      at: point)
    return (module.result(of: i), point)
  }
}

// Add similar implementations for other instructions...
