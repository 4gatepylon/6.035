package edu.mit.compilers.semantics
import edu.mit.compilers.parser._
import scala.math.BigInt

// Rule 3
case object Main {
  // Checker for proper definition of main method
  def check(n: ASTNode): Unit = {
    n match {
      case program: ASTProgram =>
        val main = program.table.maybeGlobal("main")
        main match {
          case Some(
                MethodDescr(
                  token: Token,
                  dtype: ReturnType.Type,
                  params: List[ParamDescr]
                )
              ) => {
            if (dtype != ReturnType.void)
              program.errors :+= SemanticError(
                token,
                "Main method must return void"
              )
            if (params.nonEmpty)
              program.errors :+= SemanticError(
                token,
                "Main method must not have parameters"
              )
          }
          case _ =>
            program.errors :+= SemanticError(
              program.token,
              "No main method defined"
            )
        }
      case _ =>
    }
  }
}
// Rule 2
case object Exists {
  // Checker for verifying used variables and methods have been declared
  def check(n: ASTNode): Unit = {
    n match {
      case ASTMethodName(token, id) => {
        if (!n.table.hasGlobal(id.token.string))
          n.errors :+= SemanticError(token, s"Method $id does not exist")
      }
      case ASTScalarLocation(token, id) => {
        if (!n.table.hasGlobal(id.token.string))
          n.errors :+= SemanticError(token, s"Scalar $id does not exist")
      }
      case ASTArrayLocation(token, id, expr) => {
        if (!n.table.hasGlobal(id.token.string))
          n.errors :+= SemanticError(token, s"Array $id does not exist")
      }
      case _ =>
    }
  }
}
// Rule 4
case object ArraySize {
  // Checker for ensuring arrays have size > 0
  def check(n: ASTNode): Unit = {
    n match {
      case ASTArrayDecl(token, id, size) => {
        if (size.token.string.replaceAll("0", "") == "")
          n.errors :+= SemanticError(token, s"Array $id must have nonzero size")
      }
      case _ =>
    }
  }
}
// Rule 5
case object ArgsMatchParams {
  // Checker for ensuring method call parameters match parameters declared in symbol table
  def check(n: ASTNode): Unit = {
    n match {
      case methodCall: ASTMethodCall => {
        val meth: Option[Descr] =
          n.table.maybeGlobal(methodCall.name.id.token.string)
        meth match {
          case Some(
                MethodDescr(
                  token: Token,
                  dtype: ReturnType.Type,
                  params: List[ParamDescr]
                )
              ) => {
            if (params.length != methodCall.args.length) {
              n.errors :+= SemanticError(
                token,
                s"Wrong number of arguments for method ${token.string}"
              )
            }
            methodCall.args.zipWithIndex.foreach { case (arg, i) =>
              val paramType = TypeProp.visit(arg)
              if (i < params.length && paramType != params(i).dtype) {
                n.errors :+= SemanticError(
                  token,
                  s"Wrong type for argument $i of method ${token.string}}"
                )
              }
            }
          }
          case _ =>
        }
      }
      case _ =>
    }
  }
}
// Rule 6
case object MethodExprReturns {
  // Checker for ensuring methods return values
  def check(n: ASTNode): Unit = {
    n match {
      case ASTMethodCallExpr(token, method, args) => {
        val meth: Option[Descr] = n.table.maybeGlobal(method.id.token.string)
        meth match {
          case Some(
                MethodDescr(
                  token: Token,
                  dtype: ReturnType.Type,
                  params: List[ParamDescr]
                )
              ) => {
            if (dtype == ReturnType.void)
              n.errors :+= SemanticError(
                token,
                s"Method ${token.string} does not return a value"
              )
          }
          case _ =>
        }
      }
      case _ =>
    }
  }
}
// Rule 22
case object IntLiteralSize {
  // Checker for ensuring integer literals are in bounds
  // Check spec
  // https://www.delftstack.com/howto/java/long-max-value-in-java/
  // NOTE this should be changed to Long.MaxValue probably for clarity
  val maxInt = BigInt("9223372036854775807")
  val minInt = BigInt("9223372036854775808")
  def check(n: ASTNode): Unit = {
    n match {
      case ASTIntLiteral(token) => {
        // Avoid DOS (so to speak)
        val str = token.string
        if (str.length > 19) {
          n.errors :+= SemanticError(token, s"Integer value out of bounds")
        } else {
          val bigInt = if (str.contains("0x")) {
            BigInt(str.substring(2), 16)
          } else {
            BigInt(str)
          }

          val parent = n.parent.getOrElse(throw new Exception("No parent"))
          if (parent.isInstanceOf[ASTNegExpr] && bigInt > minInt) {
            n.errors :+= SemanticError(
              token,
              s"Negative integer literal too small"
            )
          } else if (!parent.isInstanceOf[ASTNegExpr] && bigInt > maxInt) {
            n.errors :+= SemanticError(
              token,
              s"Positive integer literal too large"
            )
          }
        }
      }
      case _ =>
    }
  }
}
// Rule 7
case object NonImportParams {
  // Checker for ensuring method arguments are primitive types only
  def check(n: ASTNode): Unit = {
    n match {
      case methodCall: ASTMethodCall => {
        val meth: Option[Descr] =
          n.table.maybeGlobal(methodCall.name.id.token.string)
        meth match {
          case Some(
                MethodDescr(
                  token: Token,
                  dtype: ReturnType.Type,
                  params: List[ParamDescr]
                )
              ) => {
            // println(methodCall.table.toString())
            methodCall.args.foreach { case arg =>
              val isArray = arg match {
                case location: ASTLocation => {
                  n.table
                    .maybeGlobal(location.id.token.string)
                    .exists(_.isInstanceOf[ArrayDescr])
                }
                case _ => false
              }
              val isString = arg.isInstanceOf[ASTStringLiteral]
              if (isArray || isString)
                n.errors :+= SemanticError(
                  token,
                  "Non-imported method arguments may not be strings or arrays"
                )
            }
          }
          case _ =>
        }
      }
      case _ =>
    }
  }
}
// Rules 16, 17, 18
case object OpTypesMatch {
  // Checker for ensuring correct type of operands for various unary and binary operations
  def check(n: ASTNode): Unit = {
    n match {
      case arithOp: ASTArithOpExpr => {
        val leftType = TypeProp.visit(arithOp.left)
        val rightType = TypeProp.visit(arithOp.right)
        if (leftType != ReturnType.int || rightType != ReturnType.int) {
          n.errors :+= SemanticError(
            arithOp.token,
            "Operands of arithmetic operators must be integers"
          )
        }
      }
      case relOp: ASTRelOpExpr => {
        val leftType = TypeProp.visit(relOp.left)
        val rightType = TypeProp.visit(relOp.right)
        if (leftType != ReturnType.int || rightType != ReturnType.int) {
          n.errors :+= SemanticError(
            relOp.token,
            "Operands of relational operators must be integers"
          )
        }
      }
      case eqOp: ASTEqOpExpr => {
        val leftType = TypeProp.visit(eqOp.left)
        val rightType = TypeProp.visit(eqOp.right)
        if (
          leftType != rightType || (leftType != ReturnType.int && leftType != ReturnType.bool)
        ) {
          n.errors :+= SemanticError(
            eqOp.token,
            "Operands of equality operators must be the same type, either integers or booleans"
          )
        }
      }
      case condOp: ASTCondOpExpr => {
        val leftType = TypeProp.visit(condOp.left)
        val rightType = TypeProp.visit(condOp.right)
        if (leftType != ReturnType.bool || rightType != ReturnType.bool) {
          n.errors :+= SemanticError(
            condOp.token,
            "Operands of conditional operators must be booleans"
          )
        }
      }
      case ASTNegExpr(token, expr) => {
        TypeProp.visit(expr) match {
          case ReturnType.int =>
          case _ =>
            n.errors :+= SemanticError(
              token,
              "Operand of negation operator must be an integer"
            )
        }
      }
      case ASTNotExpr(token, expr) => {
        TypeProp.visit(expr) match {
          case ReturnType.bool =>
          case _ =>
            n.errors :+= SemanticError(
              token,
              "Operand of logical not operator must be a boolean"
            )
        }
      }
      case _ =>
    }
  }
}
// Rule 11
case object MethodExists {
  // Checker for ensuring method calls refer to declared methods or imports
  def check(n: ASTNode): Unit = {
    n match {
      case methodCall: ASTMethodCall => {
        val idDescr: Option[Descr] =
          n.table.maybeGlobal(methodCall.name.id.token.string)
        idDescr match {
          // Ignore Register and CFG
          case Some(MethodDescr(token, dtype, params)) =>
          case Some(ImportDescr(token))                =>
          case _ =>
            n.errors :+= SemanticError(
              methodCall.token,
              "Can only call methods."
            )
        }
      }
      case _ =>
    }
  }
}
// Rule 12
case object ArrayIndex {
  // Checker for ensuring indexing is only done on arrays, with integer index only
  def check(n: ASTNode): Unit = {
    n match {
      case ASTArrayLocation(token: IdToken, id: ASTIdNode, index: ASTExpr) => {
        if (TypeProp.visit(index) != ReturnType.int)
          n.errors :+= SemanticError(token, "Must index into arrays with ints")
        if (
          !n.table
            .maybeGlobal(id.token.string)
            .exists(_.isInstanceOf[ArrayDescr])
        )
          n.errors :+= SemanticError(token, "Can only index into arrays")
      }
      case _ =>
    }
  }
}
// Rule 13
case object LenArray {
  // Checker for ensuring length operator operand is an Array
  def check(n: ASTNode): Unit = {
    n match {
      case ASTLenExpr(token, id) => {
        val idDescr: Option[Descr] = n.table.maybeGlobal(id.token.string)
        idDescr match {
          case Some(ArrayDescr(token, dtype, size)) =>
          case _ =>
            n.errors :+= SemanticError(
              token,
              "Length operator must be applied to an array"
            )
        }
      }
      case _ =>
    }
  }
}
// Rule 14
case object CondBool {
  // Checker for ensuring conditions for if/for/while stmts have correct type
  def check(n: ASTNode): Unit = {
    n match {
      case ASTWhileStmt(token: Keyword, cond: ASTExpr, block: ASTBlock) => {
        if (TypeProp.visit(cond) != ReturnType.bool)
          n.errors :+= SemanticError(
            token,
            "Condition of a while loop must be boolean"
          )
      }
      case ASTIfThenStmt(
            token: Keyword,
            cond: ASTExpr,
            thenBlock: ASTBlock
          ) => {
        if (TypeProp.visit(cond) != ReturnType.bool)
          n.errors :+= SemanticError(
            token,
            "Condition of an if statement must be boolean"
          )
      }
      case ASTIfElseStmt(
            token: Keyword,
            cond: ASTExpr,
            thenBlock: ASTBlock,
            elseBlock: ASTBlock
          ) => {
        if (TypeProp.visit(cond) != ReturnType.bool)
          n.errors :+= SemanticError(
            token,
            "Condition of an if statement must be boolean"
          )
      }
      case ASTForStmt(
            token: Keyword,
            id: ASTIdNode,
            init: ASTExpr,
            cond: ASTExpr,
            update: ASTForUpdate,
            block: ASTBlock
          ) => {
        if (TypeProp.visit(cond) != ReturnType.bool)
          n.errors :+= SemanticError(
            token,
            "Condition of a for loop must be boolean"
          )
      }
      case _ =>
    }
  }
}
// Rule 21
case object InLoop {
  // Checker for ensuring break and continue stmts only occur in loops
  def check(n: ASTNode): Unit = {
    def findLoop(n: ASTNode): Option[ASTNode] = {
      n match {
        case whileASTStmt: ASTWhileStmt => Some(whileASTStmt)
        case forASTStmt: ASTForStmt     => Some(forASTStmt)
        case _ =>
          n.parent match {
            case Some(p) => findLoop(p)
            case None    => None
          }
      }
    }
    n match {
      case ASTBreakStmt(token) => {
        findLoop(n) match {
          case Some(loop) =>
          case None =>
            n.errors :+= SemanticError(
              token,
              "Break statement must be inside a loop"
            )
        }
      }
      case ASTContinueStmt(token) => {
        findLoop(n) match {
          case Some(loop) =>
          case None =>
            n.errors :+= SemanticError(
              token,
              "Continue statement must be inside a loop"
            )
        }
      }
      case _ =>
    }
  }
}
// Rules 10, 19
case object AssignTypesMatch {
  // Checker for ensuring both sides of an assignment have compatible types
  def check(n: ASTNode): Unit = {
    n match {
      case ASTEqAssignStmt(token, location, expr) => {
        val locationDescr = n.table.maybeGlobal(location.token.string)
        val exprType = TypeProp.visit(expr)
        val isArray = expr match {
          case ASTScalarLocation(token, id) =>
            n.table
              .maybeGlobal(id.token.string)
              .exists(_.isInstanceOf[ArrayDescr])
          case _ => false
        }
        locationDescr match {
          // Ignore the Register
          case Some(ScalarDescr(token, dtype)) =>
            if (dtype != exprType || isArray)
              n.errors :+= SemanticError(
                token,
                "Scalar assignment must be of the same type"
              )
          // Ignore the Register
          case Some(ArrayDescr(token, dtype, size)) =>
            if (dtype != exprType || isArray)
              n.errors :+= SemanticError(
                token,
                "Array element assignment must be of the same type"
              )
          case Some(ParamDescr(token, dtype)) =>
            if (dtype != exprType || isArray)
              n.errors :+= SemanticError(
                token,
                "ASTParam assignment must be of the same type"
              )
          // Ignore Register and CFG
          case Some(MethodDescr(token, dtype, params)) =>
            n.errors :+= SemanticError(token, "Cannot assign to a method")
          case None =>
            n.errors :+= SemanticError(
              n.token,
              "Cannot assign to an undeclared location"
            )
          case _ =>
            throw new Exception(s"Unexpected location type: $locationDescr")
        }
      }
      case ASTForStmt(token, id, init, cond, update, block) => {
        val initType = TypeProp.visit(init)
        val isArray = init match {
          case ASTScalarLocation(token, id) =>
            n.table
              .maybeGlobal(id.token.string)
              .exists(_.isInstanceOf[ArrayDescr])
          case _ => false
        }
        if (initType != ReturnType.int || isArray)
          n.errors :+= SemanticError(
            token,
            "Initial value of for statement must be of type int."
          )
      }
      case _ =>
    }
  }
}
// Rule 20
case object IncrDecrType {
  // Checker to ensure increment and decrement are done on compatible types
  def check(n: ASTNode): Unit = {
    n match {
      case op: ASTOpChange => {
        if (!op.isInstanceOf[ASTEqAssignStmt]) {
          if (op.location.isInstanceOf[ASTScalarLocation]) {
            if (
              n.table
                .maybeGlobal(op.location.id.token.string)
                .exists(_.isInstanceOf[ArrayDescr])
            )
              n.errors :+= SemanticError(
                op.token,
                "Increment and decrement assignments may only be done with integers"
              )
          }
          if (op.expr.isInstanceOf[ASTScalarLocation]) {
            val exprASTLocation = op.expr.asInstanceOf[ASTScalarLocation]
            if (
              n.table
                .getGlobal(exprASTLocation.id.token.string)
                .isInstanceOf[ArrayDescr]
            )
              n.errors :+= SemanticError(
                op.token,
                "Increment and decrement assignments may only be done with integers"
              )
          }
          if (TypeProp.visit(op.expr) != ReturnType.int)
            n.errors :+= SemanticError(
              op.token,
              "Increment and decrement assignments may only be done with integers"
            )
          if (TypeProp.visit(op.location) != ReturnType.int)
            n.errors :+= SemanticError(
              op.token,
              "Increment and decrement assignments may only be done with integers"
            )
        }
      }
      case mut: ASTMutChange => {
        if (
          n.table
            .maybeGlobal(mut.location.id.token.string)
            .exists(_.isInstanceOf[ArrayDescr]) || !(n.table
            .maybeGlobal(mut.location.id.token.string)
            .exists(_.dtype == ReturnType.int))
        )
          n.errors :+= SemanticError(
            mut.token,
            "Increment and decrement assignments may only be done with integers"
          )
      }
      case _ =>
    }
  }
}

// Rules 8 and 9
case object PropReturnType {
  // Checker to ensure methods and blocks in if/for/while stmts return correct type
  def check(n: ASTNode): Unit = {
    n match {
      case ASTVoidMethodDecl(_, _, _, block) => {
        block.returnType = ReturnType.void
      }
      case ASTTypeMethodDecl(_, typeASTNode, _, _, block) => {
        block.returnType = ReturnType.fromNode(typeASTNode)
      }
      case parentASTBlock: ASTBlock => {
        parentASTBlock.stmts.foreach {
          case ASTIfElseStmt(_, _, thenBlock, elseBlock) => {
            thenBlock.returnType = parentASTBlock.returnType
            elseBlock.returnType = parentASTBlock.returnType
          }
          case ASTIfThenStmt(_, _, thenBlock) => {
            thenBlock.returnType = parentASTBlock.returnType
          }
          case ASTForStmt(_, _, _, _, _, forASTBlock) => {
            forASTBlock.returnType = parentASTBlock.returnType
          }
          case ASTWhileStmt(_, _, whileASTBlock) => {
            whileASTBlock.returnType = parentASTBlock.returnType
          }
          case _ =>
        }
      }
      case _ =>
    }
  }
}
case object PropMustReturn {
  // Checker to ensure methods and blocks in if/for/while stmts return at all
  def check(n: ASTNode): Unit = {
    n match {
      case ASTVoidMethodDecl(token, id, params, block) => {
        block.mustReturn = false
      }
      case ASTTypeMethodDecl(token, dtype, id, params, block) => {
        block.mustReturn = true
      }
      case parentASTBlock: ASTBlock => {
        if (parentASTBlock.mustReturn && parentASTBlock.stmts.nonEmpty) {
          val lastElement: ASTStmt = parentASTBlock.stmts.last
          lastElement match {
            // Set up future recursive calls
            case ASTIfElseStmt(token, cond, thenBlock, elseBlock) => {
              thenBlock.mustReturn = parentASTBlock.mustReturn
              elseBlock.mustReturn = parentASTBlock.mustReturn
            }
            case _ =>
          }
        }
      }
      case _ =>
    }
  }
}
case object ReturnStmtsTypes {
  // Checker to ensure return statements match type in method declaration
  def check(n: ASTNode): Unit = {
    n match {
      case block: ASTBlock => {
        block.stmts.foreach {
          case stmt: ASTVoidReturnStmt => {
            if (block.returnType != ReturnType.void) {
              n.errors :+= SemanticError(
                stmt.token,
                s"Void return statement must be in a void method. $block"
              )
            }
          }
          case stmt: ASTTypeReturnStmt => {
            val expr = stmt.expr
            val exprType = TypeProp.visit(expr)
            if (exprType != block.returnType) {
              n.errors :+= SemanticError(
                stmt.token,
                s"Return type mismatches method declaration, expected ${block.returnType}, got $exprType"
              )
            }
          }
          case _ =>
        }
      }
      case _ =>
    }
  }
}
case object ReturnStmtPathsExist {
  // Checker to ensure all execution paths in a method/block have a return value
  def check(n: ASTNode): Unit = {
    n match {
      case block: ASTBlock => {
        if (block.mustReturn) {
          // If the type is not void you must have a return statement
          if (block.stmts.nonEmpty) {
            val lastElement: ASTStmt = block.stmts.last
            lastElement match {
              case ASTVoidReturnStmt(token)                         =>
              case ASTTypeReturnStmt(token, expr)                   =>
              case ASTIfElseStmt(token, cond, thenBlock, elseBlock) =>
              case _ =>
                n.errors :+= SemanticError(
                  lastElement.token,
                  s"Method has path with no return value $block"
                )
            }
          } else {
            n.errors :+= SemanticError(
              block.token,
              s"Method must return a value $block"
            )
          }
        }
      }
      case _ =>
    }
  }
}

case object Checker {
  def visit(n: ASTNode): Unit = {
    // Visitor which visits each node and runs proper semantic checkers for each node type
    Exists.check(n)
    Main.check(n)
    ArraySize.check(n)
    ArgsMatchParams.check(n)
    MethodExprReturns.check(n)
    NonImportParams.check(n)
    IntLiteralSize.check(n)
    OpTypesMatch.check(n)
    LenArray.check(n)
    InLoop.check(n)
    IncrDecrType.check(n)
    AssignTypesMatch.check(n)
    ArrayIndex.check(n)
    CondBool.check(n)
    PropReturnType.check(n)
    // Removed since correct semantics allow for paths without return, but
    // guarantee that those will fail at runtime.
    // PropMustReturn.check(n)
    ReturnStmtsTypes.check(n)
    // Removed for the same reason as PropsMustReturn (note that PropsMustReturn exists
    // solely for the correctness of ReturnStmtPathsExist)
    // ReturnStmtPathsExist.check(n)
    MethodExists.check(n)
    n match {
      case ASTProgram(token, imports, fields, methods) => {
        imports.map(visit)
        fields.map(visit)
        methods.map(visit)
      }
      case ASTImportDecl(token, id) => visit(id)
      case ASTParam(token, dtype, id) => {
        visit(dtype)
        visit(id)
      }
      case ASTScalarDecl(token, id) => visit(id)
      case ASTArrayDecl(token, id, size) => {
        visit(id)
        visit(size)
      }
      case ASTFieldDecl(token, dtype, vars) => {
        visit(dtype)
        vars.map(visit)
      }
      case ASTTypeMethodDecl(token, dtype, id, params, block) => {
        visit(dtype)
        visit(id)
        params.map(visit)
        visit(block)
      }
      case ASTVoidMethodDecl(token, id, params, block) => {
        visit(id)
        params.map(visit)
        visit(block)
      }
      case ASTBlock(token, fields, stmts) => {
        fields.map(visit)
        stmts.map(visit)
      }
      case opAssignStmt: ASTOpAssignStmt => {
        visit(opAssignStmt.location)
        visit(opAssignStmt.expr)
      }
      case mutAssignStmt: ASTMutAssignStmt => {
        visit(mutAssignStmt.location)
      }
      case ASTMethodName(token, id) => {
        visit(id)
      }
      case ASTMethodCallStmt(token, method, args) => {
        visit(method)
        args.map(visit)
      }
      case ASTIfThenStmt(token, expr, thenBlock) => {
        visit(expr)
        visit(thenBlock)
      }
      case ASTIfElseStmt(token, expr, thenBlock, elseBlock) => {
        visit(expr)
        visit(thenBlock)
        visit(elseBlock)
      }
      case ASTForStmt(token, id, init, cond, update, block) => {
        visit(id)
        visit(init)
        visit(cond)
        visit(update)
        visit(block)
      }
      case ASTWhileStmt(token, expr, block) => {
        visit(expr)
        visit(block)
      }
      case ASTTypeReturnStmt(token, expr) => visit(expr)
      case opForUpdate: ASTOpForUpdate => {
        visit(opForUpdate.location)
        visit(opForUpdate.expr)
      }
      case mutForUpdate: ASTMutForUpdate => {
        visit(mutForUpdate.location)
      }
      case ASTScalarLocation(token, id) => {
        visit(id)
      }
      case ASTArrayLocation(token, id, expr) => {
        visit(id)
        visit(expr)
      }
      case ASTMethodCallExpr(token, method, args) => {
        visit(method)
        args.map(visit)
      }
      case ASTLenExpr(token, expr) => visit(expr)
      case binOpExpr: ASTBinOpExpr => {
        visit(binOpExpr.left)
        visit(binOpExpr.right)
      }
      case ASTNegExpr(token, expr)   => visit(expr)
      case ASTNotExpr(token, expr)   => visit(expr)
      case ASTParenExpr(token, expr) => visit(expr)
      case _                         =>
    }
  }
}
