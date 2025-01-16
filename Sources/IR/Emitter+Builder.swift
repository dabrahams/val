import FrontEnd

extension Emitter {
  /// Gets the current builder context.
  private var builderContext: BuilderContext {
    BuilderContext(
      module: module,
      insertionPoint: insertionPoint!, 
      diagnostics: diagnostics)
  }

  /// Builds IR using the builder DSL.
  private mutating func build(@IRBuilder content: () -> [BuilderInstruction]) -> Operand? {
    var context = builderContext
    let result = context.build(content)
    module = context.module
    insertionPoint = context.insertionPoint
    diagnostics = context.diagnostics
    return result
  }

  /// Example of simplified IR emission using the builder
  private mutating func emitMoveBuiltInWithBuilder(
    _ value: Operand, to storage: Operand, at site: SourceRange
  ) {
    _ = build {
      Access(effect: .set, operand: storage, site: site)
      Access(effect: .sink, operand: value, site: site)
      Load(operand: value, site: site) 
      Store(value: value, target: storage, site: site)
      EndAccess(operand: value, site: site)
      EndAccess(operand: storage, site: site)
    }
  }
}
