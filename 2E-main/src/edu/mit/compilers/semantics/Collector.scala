package edu.mit.compilers.semantics

import edu.mit.compilers.parser._

// Error merge object (keeps track of errors for each node type and collects them for subtrees)
case object Collector {
  def visit(n: ASTNode): List[SemanticError] = {
    n match {
      case program: ASTProgram => {
        var newErrors = program.errors
        for (i <- program.imports) {
          newErrors ++= visit(i)
        }
        for (f <- program.fields) {
          newErrors ++= visit(f)
        }
        for (m <- program.methods) {
          newErrors ++= visit(m)
        }
        newErrors
      }
      case importDecl: ASTImportDecl => {
        var newErrors = importDecl.errors
        newErrors ++= visit(importDecl.id)
        newErrors
      }
      case fieldDecl: ASTFieldDecl => {
        var newErrors = fieldDecl.errors
        for (v <- fieldDecl.vars) {
          newErrors ++= visit(v)
        }
        newErrors
      }
      case scalarDecl: ASTScalarDecl => {
        var newErrors = scalarDecl.errors
        newErrors ++= visit(scalarDecl.id)
        newErrors
      }
      case arrayDecl: ASTArrayDecl => {
        var newErrors = arrayDecl.errors
        newErrors ++= visit(arrayDecl.id)
        newErrors ++= visit(arrayDecl.size)
        newErrors
      }
      case param: ASTParam => {
        var newErrors = param.errors
        newErrors ++= visit(param.dtype)
        newErrors ++= visit(param.id)
        newErrors
      }
      case typeMethodDecl: ASTTypeMethodDecl => {
        var newErrors = typeMethodDecl.errors
        newErrors ++= visit(typeMethodDecl.dtype)
        newErrors ++= visit(typeMethodDecl.id)
        for (p <- typeMethodDecl.params) {
          newErrors ++= visit(p)
        }
        newErrors ++= visit(typeMethodDecl.block)
        newErrors
      }
      case voidMethodDecl: ASTVoidMethodDecl => {
        var newErrors = voidMethodDecl.errors
        newErrors ++= visit(voidMethodDecl.id)
        for (p <- voidMethodDecl.params) {
          newErrors ++= visit(p)
        }
        newErrors ++= visit(voidMethodDecl.block)
        newErrors
      }
      case block: ASTBlock => {
        var newErrors = block.errors
        for (f <- block.fields) {
          newErrors ++= visit(f)
        }
        for (s <- block.stmts) {
          newErrors ++= visit(s)
        }
        newErrors
      }
      case ifThenStmt: ASTIfThenStmt => {
        var newErrors = ifThenStmt.errors
        newErrors ++= visit(ifThenStmt.cond)
        newErrors ++= visit(ifThenStmt.thenBlock)
        newErrors
      }
      case ifElseStmt: ASTIfElseStmt => {
        var newErrors = ifElseStmt.errors
        newErrors ++= visit(ifElseStmt.cond)
        newErrors ++= visit(ifElseStmt.thenBlock)
        newErrors ++= visit(ifElseStmt.elseBlock)
        newErrors
      }
      case forStmt: ASTForStmt => {
        var newErrors = forStmt.errors
        newErrors ++= visit(forStmt.id)
        newErrors ++= visit(forStmt.init)
        newErrors ++= visit(forStmt.cond)
        newErrors ++= visit(forStmt.update)
        newErrors ++= visit(forStmt.block)
        newErrors
      }
      case whileStmt: ASTWhileStmt => {
        var newErrors = whileStmt.errors
        newErrors ++= visit(whileStmt.cond)
        newErrors ++= visit(whileStmt.block)
        newErrors
      }
      case opAssignStmt: ASTOpAssignStmt => {
        var newErrors = opAssignStmt.errors
        newErrors ++= visit(opAssignStmt.location)
        newErrors ++= visit(opAssignStmt.expr)
        newErrors
      }
      case mutAssignStmt: ASTMutAssignStmt => {
        var newErrors = mutAssignStmt.errors
        newErrors ++= visit(mutAssignStmt.location)
        newErrors
      }
      case methodName: ASTMethodName => {
        var newErrors = methodName.errors
        newErrors ++= visit(methodName.id)
        newErrors
      }
      case methodCallStmt: ASTMethodCallStmt => {
        var newErrors = methodCallStmt.errors
        newErrors ++= visit(methodCallStmt.name)
        for (a <- methodCallStmt.args) {
          newErrors ++= visit(a)
        }
        newErrors
      }
      case voidReturnStmt: ASTVoidReturnStmt => {
        voidReturnStmt.errors
      }
      case typeReturnStmt: ASTTypeReturnStmt => {
        var newErrors = typeReturnStmt.errors
        newErrors ++= visit(typeReturnStmt.expr)
        newErrors
      }
      case breakStmt: ASTBreakStmt => {
        breakStmt.errors
      }
      case continueStmt: ASTContinueStmt => {
        continueStmt.errors
      }
      case opForUpdate: ASTOpForUpdate => {
        var newErrors = opForUpdate.errors
        newErrors ++= visit(opForUpdate.location)
        newErrors ++= visit(opForUpdate.expr)
        newErrors
      }
      case mutForUpdate: ASTMutForUpdate => {
        var newErrors = mutForUpdate.errors
        newErrors ++= visit(mutForUpdate.location)
        newErrors
      }
      case scalarLocation: ASTScalarLocation => {
        var newErrors = scalarLocation.errors
        newErrors ++= visit(scalarLocation.id)
        newErrors
      }
      case arrayLocation: ASTArrayLocation => {
        var newErrors = arrayLocation.errors
        newErrors ++= visit(arrayLocation.id)
        newErrors ++= visit(arrayLocation.index)
        newErrors
      }
      case methodCallExpr: ASTMethodCallExpr => {
        var newErrors = methodCallExpr.errors
        newErrors ++= visit(methodCallExpr.name)
        for (a <- methodCallExpr.args) {
          newErrors ++= visit(a)
        }
        newErrors
      }
      case literal: ASTLiteral => {
        literal.errors
      }
      case stringLiteral: ASTStringLiteral => {
        stringLiteral.errors
      }
      case lenExpr: ASTLenExpr => {
        var newErrors = lenExpr.errors
        newErrors ++= visit(lenExpr.id)
        newErrors
      }
      case binOpExpr: ASTBinOpExpr => {
        var newErrors = binOpExpr.errors
        newErrors ++= visit(binOpExpr.left)
        newErrors ++= visit(binOpExpr.right)
        newErrors
      }
      case negExpr: ASTNegExpr => {
        var newErrors = negExpr.errors
        newErrors ++= visit(negExpr.expr)
        newErrors
      }
      case notExpr: ASTNotExpr => {
        var newErrors = notExpr.errors
        newErrors ++= visit(notExpr.expr)
        newErrors
      }
      case parenExpr: ASTParenExpr => {
        var newErrors = parenExpr.errors
        newErrors ++= visit(parenExpr.expr)
        newErrors
      }
      case typeNode: ASTTypeNode => {
        typeNode.errors
      }
      case idNode: ASTIdNode => {
        idNode.errors
      }
      case node: ASTNode =>
        throw new Exception("Unhandled node type: " + node.getClass.getName)
    }
  }
}
