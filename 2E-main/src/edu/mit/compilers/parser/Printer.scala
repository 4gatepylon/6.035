package edu.mit.compilers.parser

case object Printer {
  def visit(n: ASTNode): String = {
    val strs: List[String] = (n match {
      case ASTProgram(token, imports, fields, methods) =>
        "<program>" :: imports.map(visit) ++ fields.map(
          visit
        ) ++ methods.map(
          visit
        )
      case ASTImportDecl(token, id) =>
        List("<import_decl>", visit(id))
      case ASTParam(token, dtype, id) =>
        List("<param>", visit(dtype), visit(id))
      case ASTScalarDecl(token, id) =>
        List("<scalar_decl>", visit(id))
      case ASTArrayDecl(token, id, size) =>
        List("<array_decl>", visit(id), visit(size))
      case ASTFieldDecl(token, dtype, vars) =>
        List("<field_decl>", visit(dtype)) ++ vars.map(visit)
      case ASTTypeMethodDecl(token, dtype, id, params, block) =>
        List("<method_decl>", visit(dtype), visit(id)) ++ params
          .map(
            visit
          ) :+ visit(block)
      case ASTVoidMethodDecl(token, id, params, block) =>
        List("<method_decl>", visit(id)) ++ params.map(
          visit
        ) :+ visit(block)
      case ASTBlock(token, fields, stmts) =>
        "<block>" :: fields.map(visit) ++ stmts.map(visit)
      case ASTEqAssignStmt(token, location, expr) =>
        List("<assign_op>: =", visit(location), visit(expr))
      case ASTAddAssignStmt(token, location, expr) =>
        List("<compound_assign_op>: +=", visit(location), visit(expr))
      case ASTSubAssignStmt(token, location, expr) =>
        List("<compound_assign_op>: -=", visit(location), visit(expr))
      case ASTIncrStmt(token, location) =>
        List("<increment>: ++", visit(location))
      case ASTDecrStmt(token, location) =>
        List("<increment>: --", visit(location))
      case ASTMethodName(token, id) =>
        List("<method_name>", visit(id))
      case ASTMethodCallStmt(token, method, args) =>
        List("<method_call>", visit(method)) ++ args.map(
          visit
        )
      case ASTIfThenStmt(token, expr, thenBlock) =>
        List("`if`", visit(expr), visit(thenBlock))
      case ASTIfElseStmt(token, expr, thenBlock, elseBlock) =>
        List("`if`", visit(expr), visit(thenBlock), visit(elseBlock))
      case ASTForStmt(token, id, init, cond, update, block) =>
        List(
          "`for`",
          visit(id),
          visit(init),
          visit(cond),
          visit(update),
          visit(block)
        )
      case ASTWhileStmt(token, expr, block) =>
        List("`while`", visit(expr), visit(block))
      case ASTVoidReturnStmt(token) =>
        List("<return>")
      case ASTTypeReturnStmt(token, expr) =>
        List("<return>", visit(expr))
      case ASTBreakStmt(token) =>
        List("`break`")
      case ASTContinueStmt(token) =>
        List("`continue`")
      case ASTAddForUpdate(token, location, expr) =>
        List("<for_update>", visit(location), visit(expr))
      case ASTSubForUpdate(token, location, expr) =>
        List("<for_update>", visit(location), visit(expr))
      case ASTIncrForUpdate(token, location) =>
        List("<for_update>", visit(location))
      case ASTDecrForUpdate(token, location) =>
        List("<for_update>", visit(location))
      case ASTScalarLocation(token, id) =>
        List("<location>", visit(id))
      case ASTArrayLocation(token, id, expr) =>
        List("<location>", visit(id), visit(expr))
      case ASTMethodCallExpr(token, method, args) =>
        List("<method_call>", visit(method)) ++ args.map(
          visit
        )
      case ASTIntLiteral(token) =>
        List("<int_literal>: " + token.string)
      case ASTCharLiteral(token) =>
        List("<char_literal>: " + token.string)
      case ASTBoolLiteral(token) =>
        List("<bool_literal>: " + token.string)
      case ASTStringLiteral(token) =>
        List("<string_literal>: " + token.string)
      case ASTLenExpr(token, expr) =>
        List("len", visit(expr))
      case ASTAddExpr(token, left, right) =>
        List("<bin_op>: +", visit(left), visit(right))
      case ASTSubExpr(token, left, right) =>
        List("<bin_op>: -", visit(left), visit(right))
      case ASTMulExpr(token, left, right) =>
        List("<bin_op>: *", visit(left), visit(right))
      case ASTDivExpr(token, left, right) =>
        List("<bin_op>: /", visit(left), visit(right))
      case ASTModExpr(token, left, right) =>
        List("<bin_op>: %", visit(left), visit(right))
      case ASTLtExpr(token, left, right) =>
        List("<bin_op>: <", visit(left), visit(right))
      case ASTGtExpr(token, left, right) =>
        List("<bin_op>: >", visit(left), visit(right))
      case ASTLeExpr(token, left, right) =>
        List("<bin_op>: <=", visit(left), visit(right))
      case ASTGeExpr(token, left, right) =>
        List("<bin_op>: >=", visit(left), visit(right))
      case ASTEqExpr(token, left, right) =>
        List("<bin_op>: ==", visit(left), visit(right))
      case ASTNeExpr(token, left, right) =>
        List("<bin_op>: !=", visit(left), visit(right))
      case ASTAndExpr(token, left, right) =>
        List("<bin_op>: &&", visit(left), visit(right))
      case ASTOrExpr(token, left, right) =>
        List("<bin_op>: ||", visit(left), visit(right))
      case ASTNegExpr(token, expr) =>
        List("<un_op>: -", visit(expr))
      case ASTNotExpr(token, expr) =>
        List("<un_op>: !", visit(expr))
      case ASTParenExpr(token, expr) =>
        List("<seq>", visit(expr))
      case ASTTypeNode(token) =>
        List("<type>: " + token.string)
      case ASTIdNode(token) =>
        List("<id>: " + token.string)
    })
    (strs ++ n.errors.map(_.toString))
      .filter(_.nonEmpty)
      .mkString("\n")
      .replace("\n", "\n  ")
  }
}
