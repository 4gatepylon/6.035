package edu.mit.compilers.codegen

import edu.mit.compilers.parser._

object ShortCircuit {
  // Visitor for short circuiting AST boolean expressions into fork blocks
  def expr(
      c: ASTExpr,
      condStmt: ASTCondStmt,
      t: BasicBlock,
      f: BasicBlock,
      depth: Int,
      loop: Option[ForkBlock]
  ): ForkBlock = {
    c match {
      case ASTAndExpr(token, c1, c2) => {
        val b2 = expr(c2, condStmt, t, f, depth, loop)
        val b1 = expr(c1, condStmt, b2, f, depth, loop)
        b1
      }
      case ASTOrExpr(token, c1, c2) => {
        val b2 = expr(c2, condStmt, t, f, depth, loop)
        val b1 = expr(c1, condStmt, t, b2, depth, loop)
        b1
      }
      case ASTNotExpr(token, c1) => {
        val b1 = expr(c1, condStmt, f, t, depth, loop)
        b1
      }
      case _ => {
        val fork = BlockMaker.fork(c, condStmt, t, f, depth, loop)
        fork
      }
    }
  }
}
