package edu.mit.compilers.semantics
import edu.mit.compilers.parser._

// Type propagator for expressions
case object TypeProp {
  // NOTE: this presupposes that you have confirmed that
  // all the children are valid (typechecking is not the same
  // as typeprop, typeprop is used for typechecking, but
  // not vice-versa... typechecking enables usto then use
  // further typeprop)
  def visit(n: ASTNode): ReturnType.Type = {
    n match {
      case ASTScalarLocation(token, id) =>
        visit(id)
      case ASTArrayLocation(token, id, expr) =>
        visit(id)
      case ASTMethodCallExpr(token, method, args) =>
        visit(method)
      case ASTMethodName(token, id) =>
        visit(id)
      case ASTIntLiteral(token) =>
        ReturnType.int
      case ASTCharLiteral(token) =>
        ReturnType.int
      case ASTBoolLiteral(token) =>
        ReturnType.bool
      case ASTStringLiteral(token) =>
        ReturnType.void
      case ASTLenExpr(token, expr) =>
        ReturnType.int
      case arithOpExpr: ASTArithOpExpr =>
        ReturnType.int
      case relOpExpr: ASTRelOpExpr =>
        ReturnType.bool
      case eqOpExpr: ASTEqOpExpr =>
        ReturnType.bool
      case condOpExpr: ASTCondOpExpr =>
        ReturnType.bool
      case ASTNegExpr(token, expr) =>
        ReturnType.int
      case ASTNotExpr(token, expr) =>
        ReturnType.bool
      case ASTParenExpr(token, expr) =>
        visit(expr)
      case ASTIdNode(token) =>
        val descr = n.table.maybeGlobal(token.string)
        descr match {
          case Some(_) => descr.get.dtype
          case None    => throw new Exception("not found")//ReturnType.void
        }
      // NOTE: we do not support propagation on statements (for example).
      // The parser guarantees that once in an expression, things under it
      // are of the node types above. We only do type propagation for expressions
      // though we do typechecking for statements (i.e. `a = b` requires that `a` and
      // `b` have the same type, but to propagate we only do for `a` (unlikely) and
      // for `b`, but never for `a = b`).
      case _ =>
        throw new Exception("Type does not support propagation")
    }
  }
}
