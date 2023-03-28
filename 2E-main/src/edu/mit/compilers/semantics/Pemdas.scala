package edu.mit.compilers.semantics

import edu.mit.compilers.parser._

object Pemdas {
  // To simplify our code below
  def getRightOrExpr(opExpr: ASTOpExpr): ASTExpr = {
    opExpr match {
      case binOpExpr: ASTBinOpExpr => binOpExpr.right
      case unOpExpr: ASTUnOpExpr   => unOpExpr.expr
      case unkOp: ASTExpr =>
        throw new Exception(s"Not an ASTOpExpr in `getRight`: $unkOp")
    }
  }
  def getLeftOrExpr(opExpr: ASTOpExpr): ASTExpr = {
    opExpr match {
      case binOpExpr: ASTBinOpExpr => binOpExpr.left
      case unOpExpr: ASTUnOpExpr   => unOpExpr.expr
      case unkOp: ASTExpr =>
        throw new Exception(s"Not an ASTOpExpr in `getLeft`: $unkOp")
    }
  }
  def setRightOrExpr(opExpr: ASTOpExpr, newRight: ASTExpr): Unit = {
    opExpr match {
      case binOpExpr: ASTBinOpExpr => binOpExpr.right = newRight
      case unOpExpr: ASTUnOpExpr   => unOpExpr.expr = newRight
      case unkOp: ASTExpr =>
        throw new Exception(s"Not an ASTOpExpr in `setRight`: $unkOp")
    }
  }
  def setLeftOrExpr(opExpr: ASTOpExpr, newLeft: ASTExpr): Unit = {
    opExpr match {
      case binOpExpr: ASTBinOpExpr => binOpExpr.left = newLeft
      case unOpExpr: ASTUnOpExpr   => unOpExpr.expr = newLeft
      case unkOp: ASTExpr =>
        throw new Exception(s"Not an ASTOpExpr in `setLeft`: $unkOp")
    }
  }

  // Orders a tree by PEMDAS
  // Important for x-legal-12
  // Order:
  //     -
  //     !
  //     * / %
  //     + -
  //     < <= >= >
  //     == !=
  //     &&
  //     ||
  def precedence(n: ASTNode): Int = {
    8 - (n match {
      // If you hit one of these "singletons"
      // anything below them is just what it is
      case _: ASTLiteral        => 0
      case _: ASTLocation       => 0
      case ASTParenExpr(_, _)   => 0
      case _: ASTMethodCallExpr => 0
      case ASTLenExpr(_, _)     => 0
      // If you hit one of these, you need to go down the
      // left subtree of your right subtree
      case ASTNegExpr(_, _)    => 1
      case ASTNotExpr(_, _)    => 2
      case ASTMulExpr(_, _, _) => 3
      case ASTDivExpr(_, _, _) => 3
      case ASTModExpr(_, _, _) => 3
      case ASTAddExpr(_, _, _) => 4
      case ASTSubExpr(_, _, _) => 4
      case ASTLtExpr(_, _, _)  => 5
      case ASTLeExpr(_, _, _)  => 5
      case ASTGeExpr(_, _, _)  => 5
      case ASTGtExpr(_, _, _)  => 5
      case ASTEqExpr(_, _, _)  => 6
      case ASTNeExpr(_, _, _)  => 6
      case ASTAndExpr(_, _, _) => 7
      case ASTOrExpr(_, _, _)  => 8
      case x: ASTExpr =>
        throw new Exception(
          s"Unknown expression type $x for PEMDAS in `precedence`"
        )
    })
  }

  // Returns the leftmost child of a tree that has at most the precedence given
  // (that is, all nodes to the left have higher precedence)
  def leftmostChildAtMost(n: ASTNode, p: Int): ASTOpExpr = {
    n match {
      case _: ASTLiteral =>
        throw new Exception("Cannot find leftmost child of ASTIntLiteral")
      case ASTIdNode(_) =>
        throw new Exception("Cannot find leftmost child of ASTIdNode")
      case ASTParenExpr(_, _) =>
        throw new Exception("Cannot find leftmost child of ASTParenExpr")
      case unOpExpr: ASTUnOpExpr => {
        if (precedence(unOpExpr.expr) <= p) {
          leftmostChildAtMost(unOpExpr.expr, p)
        } else {
          unOpExpr
        }
      }
      case binOpExpr: ASTBinOpExpr => {
        if (precedence(binOpExpr.left) <= p) {
          leftmostChildAtMost(binOpExpr.left, p)
        } else {
          binOpExpr
        }
      }
      case x: ASTExpr =>
        throw new Exception(s"Unknown expression type $x for PEMDAS")
    }
  }

  // Returns ordered version of the tree after mutatings

  // Maintains the invariant that in-order traversal is the same, while
  // the order of computation (bottom of tree up) will be correct given
  // PEMDAS. It guarantees that along any path down the tree, the precedence
  // level monotonically increases (or stays the same) and that in the cases
  // where precedence was the same, the order of evaluation is left to right
  // (i.e. a plus will be sent down before a minus for example).

  // Note that we assume that the tree is right-recursive
  // Our algorithm is as follows:
  //   this.right = orderedExpr(this.right)
  //   this.left = orderedExpr(this.left)
  //   parent = parentOf

  def orderExpr(n: ASTExpr): ASTExpr = {
    n match {
      // Singletons we can just return
      case x: ASTLiteral        => x
      case x: ASTLenExpr        => x
      case x: ASTScalarLocation => x
      // Nested expressions that are singletons need to be seperately
      // ordered
      case methodCallExpr: ASTMethodCallExpr => {
        methodCallExpr.args = methodCallExpr.args.map(_ match {
          case expr: ASTExpr    => orderExpr(expr)
          case nonExpr: ASTNode => nonExpr
        })
        methodCallExpr
      }
      case arrayLocation: ASTArrayLocation => {
        arrayLocation.index = orderExpr(arrayLocation.index)
        arrayLocation
      }
      case parenExpr: ASTParenExpr => {
        parenExpr.expr = orderExpr(parenExpr.expr)
        parenExpr
      }
      case opExpr: ASTOpExpr => {
        val orderedRight = orderExpr(getRightOrExpr(opExpr))
        val rootPrecedence = precedence(opExpr)
        var retValue: ASTExpr = opExpr
        // Unary operators and binary operators need to be treated similarly:
        // rotate up lower precedence nodes to the top (in equals case we want
        // computation to go left to right)
        if (precedence(orderedRight) <= rootPrecedence) {
          // Find the first node which can be a parent of root
          val rightLeftmost = leftmostChildAtMost(orderedRight, rootPrecedence)
          // If the root is an unOp and the rightLeftmost is a binOp:
          //   root's expr = rightLeftmost's left
          //   rightLeftmost's left = root
          //
          // If the root is an unOp and the rightLeftmost is a unOp:
          //  root's expr = rightLeftmost's expr
          //  rightLeftmost's expr = root
          //
          // If the root is a binOp and the rightLeftmost is a binOp:
          //  root's right = rightLeftmost's left
          //  rightLeftmost's left = root
          //
          // If the root is a binOp and the rightLeftmost is a unOp:
          // root's right = rightLeftmost's expr
          // rightLeftmost's expr = root
          //
          // ALWAYS: return orderedRight
          setRightOrExpr(opExpr, getLeftOrExpr(rightLeftmost))
          setLeftOrExpr(rightLeftmost, opExpr)
          retValue = orderedRight
        } else {
          opExpr match {
            case binOpExpr: ASTBinOpExpr =>
              binOpExpr.right = orderedRight
            case unOpExpr: ASTUnOpExpr =>
              unOpExpr.expr = orderedRight
            case unkOp: ASTExpr =>
              throw new Exception(
                s"Unknown expression type $unkOp for PEMDAS in `orderedExpr` where no swap necessary"
              )
          }
          retValue = opExpr
        }
        if (opExpr.isInstanceOf[ASTBinOpExpr]) {
          // In the case of a bin-op expression, only if the left is
          // a parenthetical, it might not be otherwise ordered
          // ...This ALWAYS has to be done
          setLeftOrExpr(opExpr, orderExpr(getLeftOrExpr(opExpr)))
        }
        retValue
      }
      case x: ASTExpr =>
        throw new Exception(
          s"Unknown expression type $x for PEMDAS in `orderedExpr`"
        )
    }
  }

  def order(n: ASTNode): Unit = {
    n match {
      // This recurses down the tree to find expressions and then
      // sets their parents' expression pointer to point to an ordered expression
      // of the same expression. That is done using `orderExpr` which orders the
      // expression (mutates) and then returns the new root.
      case opAssignStmt: ASTOpAssignStmt => {
        // Set
        opAssignStmt.location =
          orderExpr(opAssignStmt.location).asInstanceOf[ASTLocation]
        opAssignStmt.expr = orderExpr(opAssignStmt.expr)
      }
      case ifThenStmt: ASTIfThenStmt => {
        // Set
        ifThenStmt.cond = orderExpr(ifThenStmt.cond)

        // Search
        order(ifThenStmt.thenBlock)
      }
      case ifElseStmt: ASTIfElseStmt => {
        // Set
        ifElseStmt.cond = orderExpr(ifElseStmt.cond)

        // Search
        order(ifElseStmt.thenBlock)
        order(ifElseStmt.elseBlock)
      }
      case forStmt: ASTForStmt => {
        // Set
        forStmt.init = orderExpr(forStmt.init)
        forStmt.cond = orderExpr(forStmt.cond)

        // Search
        order(forStmt.update)
        order(forStmt.block)
      }
      case whileStmt: ASTWhileStmt => {
        // Set
        whileStmt.cond = orderExpr(whileStmt.cond)

        // Search
        order(whileStmt.block)
      }
      case opForUpdate: ASTOpForUpdate => {
        // Set
        opForUpdate.expr = orderExpr(opForUpdate.expr)
        opForUpdate.location =
          orderExpr(opForUpdate.location).asInstanceOf[ASTLocation]
      }
      case mutAssignStmt: ASTMutAssignStmt => {
        // Set
        mutAssignStmt.location =
          orderExpr(mutAssignStmt.location).asInstanceOf[ASTLocation]
      }
      case typeReturnStmt: ASTTypeReturnStmt => {
        // Set
        typeReturnStmt.expr = orderExpr(typeReturnStmt.expr)
      }
      case mutForUpdate: ASTMutForUpdate => {
        // Set
        mutForUpdate.location =
          orderExpr(mutForUpdate.location).asInstanceOf[ASTLocation]
      }
      case methodCallStmt: ASTMethodCallStmt => {
        // Set and Search
        methodCallStmt.args = methodCallStmt.args.map(_ match {
          case expr: ASTExpr    => orderExpr(expr)
          case nonExpr: ASTNode => nonExpr
        })
      }
      // Search
      case ASTProgram(_, _, _, methods)         => methods.foreach(order)
      case ASTTypeMethodDecl(_, _, _, _, block) => order(block)
      case ASTVoidMethodDecl(_, _, _, block)    => order(block)
      case ASTBlock(_, _, stmts)                => stmts.foreach(order)
      // All expressions should be captured into an `orderExpression`
      case expr: ASTExpr =>
        throw new Exception(
          s"You should only call `orderExpr` on expressions NOT `order`, called `order` on $expr"
        )
      case _ =>
    }
  }
}
