import Utils

/// The type checker for Val's statements.
struct StmtChecker: StmtVisitor {

  typealias StmtResult = Bool

  /// The declaration space in which the visited statements reside.
  var useSite: DeclSpace

  /// The policy to adopt for substituting free type variables.
  let freeVarSubstPolicy: FreeTypeVarSubstPolicy

  mutating func visit(_ node: BraceStmt) -> Bool {
    let oldUseSite = useSite
    useSite = node
    defer { useSite = oldUseSite }

    var success = true
    for i in 0 ..< node.stmts.count {
      switch node.stmts[i] {
      case let decl as Decl:
        success = TypeChecker.check(decl: decl) && success

      case let stmt as Stmt:
        success = stmt.accept(&self) && success

      case var expr as Expr:
        success = TypeChecker.check(
          expr: &expr, useSite: node, freeTypeVarSubstPolicy: freeVarSubstPolicy) && success
        node.stmts[i] = expr

      default:
        fatalError("unexpected node")
      }
    }

    return success
  }

  mutating func visit(_ node: RetStmt) -> Bool {
    // Use the return type of the function as the fixed type of the returned value.
    let funDecl = node.funDecl ?? fatalError("return statement outside of a function")
    assert(funDecl.state >= .realized)
    let funType = funDecl.type as! FunType
    var fixedRetType = funType.retType

    if fixedRetType[.hasTypeParams] {
      if let env = funDecl.prepareGenericEnv() {
        (fixedRetType, _) = env.contextualize(fixedRetType, from: useSite)
      } else {
        fixedRetType = funType.context.errorType
      }
    }

    // Type check the returned value.
    if var value = node.value {
      // Type check the returned expression.
      let success = TypeChecker.check(expr: &value, fixedType: fixedRetType, useSite: useSite)
      node.value = value
      return success
    } else {
      return true
    }
  }

  mutating func visit(_ node: IfStmt) -> Bool {
    let context = node.condition.type.context

    // The condition must have a Boolean type.
    let bool = context.getTypeDecl(for: .Bool)!.instanceType
    var success = TypeChecker.check(expr: &node.condition, fixedType: bool, useSite: useSite)

    success = node.thenBody.accept(&self) && success
    if let elseBody = node.elseBody {
      success = elseBody.accept(&self) && success
    }

    return success
  }

  func visit(_ node: MatchCaseStmt) -> Bool {
    fatalError("unreachable")
  }

}
