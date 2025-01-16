import FrontEnd
import Utils

extension Module {

  /// Builds an instruction sequence and inserts it at the current insertion point
  mutating func buildBlock(
    at point: InsertionPoint,
    @IRBuilder _ content: () -> [BuilderInstruction]
  ) -> (lastResult: Operand?, finalPoint: InsertionPoint) {
    var ctx = BuilderContext(module: self, insertionPoint: point)
    let result = ctx.build(content)
    self = ctx.module
    return (result, ctx.insertionPoint)
  }

}
