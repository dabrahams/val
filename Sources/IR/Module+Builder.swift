import FrontEnd
import Utils

extension Module {
  /// Builds and inserts an instruction sequence at a specific location
  mutating func insert(
    at point: InsertionPoint,
    site: SourceRange,
    @IRBuilder content: () -> [BuilderInstruction]
  ) -> Operand? {
    var ctx = BuilderContext(module: self, insertionPoint: point)
    return ctx.build(at: site, content)
  }
}
