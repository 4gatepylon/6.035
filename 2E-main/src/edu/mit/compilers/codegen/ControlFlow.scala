package edu.mit.compilers.codegen

import edu.mit.compilers.parser._
import edu.mit.compilers.optimization._

object MethodToCFG {
  def algSimpNode(n: ASTNode): ASTNode = {
    n match {
      case opChange: ASTOpChange => {
        opChange.location =
          algSimpNode(opChange.location).asInstanceOf[ASTLocation]
        opChange.expr = algSimpNode(opChange.expr).asInstanceOf[ASTExpr]
        opChange
      }
      case mutChange: ASTMutChange => {
        mutChange.location =
          algSimpNode(mutChange.location).asInstanceOf[ASTLocation]
        mutChange
      }
      case methodCall: ASTMethodCall => {
        methodCall.args =
          methodCall.args.map(algSimpNode).map(_.asInstanceOf[ASTImportArg])
        methodCall
      }
      case ASTTypeReturnStmt(token, expr) =>
        ASTTypeReturnStmt(token, algSimpNode(expr).asInstanceOf[ASTExpr])
      case ASTArrayLocation(token, id, expr) =>
        ASTArrayLocation(
          token,
          id,
          algSimpNode(expr).asInstanceOf[ASTExpr]
        )
      case binOpExpr: ASTBinOpExpr => {
        binOpExpr.left = algSimpNode(binOpExpr.left).asInstanceOf[ASTExpr]
        binOpExpr.right = algSimpNode(binOpExpr.right).asInstanceOf[ASTExpr]
        binOpExpr
      }
      case ASTNegExpr(token, expr) => {
        val subExpr = algSimpNode(expr).asInstanceOf[ASTExpr]
        subExpr match {
          // - - x = x
          case subNeg: ASTNegExpr => subNeg.expr
          case _                  => ASTNegExpr(token, subExpr)
        }
      }
      case ASTNotExpr(token, expr) => {
        val subExpr = algSimpNode(expr).asInstanceOf[ASTExpr]
        subExpr match {
          // ! ! x = x
          case subNot: ASTNotExpr => subNot.expr
          case _                  => ASTNotExpr(token, subExpr)
        }
      }
      case ASTParenExpr(token, expr) => {
        // parentheses no longer needed since AST already encodes order of operations
        algSimpNode(expr).asInstanceOf[ASTExpr]
      }
      case _ => n
    }
  }

  def convert(
      method: ASTMethodDecl,
      globalTable: Option[RegTable]
  ): BasicBlock = {
    // Destruct and merge

    // depth starts at 1 since the global table is depth 0
    // loop is None since there is no encapsulating loop at this level
    val (depth, loop) = (1, None)
    val (begin, end) = Destruct.tree(method.block, depth, loop)
    Destruct.reroute(begin)
    val merged = BlockMerger.all(begin)

    // Initialize table for params and fields; load params to it
    val paramTable = Some(RegTable(parent = globalTable))
    paramTable.get.loadParams(method.params)

    // Insert/create all the registers used by variables (named)
    // into the table to use when flattening
    RegTableInserter.all(merged, paramTable)

    // Set whether the method is typed or not per block
    val methodIsTyped = method.isInstanceOf[ASTTypeMethodDecl]
    merged.visit(block => block.methodIsTyped = methodIsTyped)
    merged.visit(block => block.nodes = block.nodes.map(algSimpNode))

    // Create sequence of instructions
    Flattener.cfg(merged)

    // Swap registers from doing temp = x; a = temp; to a = x; temp = a
    RegSwap.cfg(merged)

    // Make sure that the parameters are available for the first basic block (the function header)
    merged.visit[Unit](bb => bb.functionHeader = Some(merged))

    // Make sure that all instructions have their basic block available
    merged.visit[Unit](bb => {
      bb.instrs.foreach(instr => instr.block = Some(bb))
    })

    merged
  }
}

class CFGManager(root: ASTNode) {
  // Maps method names to the CFGs that they will use
  val program = root.asInstanceOf[ASTProgram]
  val globalTable: RegTable = RegTable(parent = None)
  var cfgs: Map[String, BasicBlock] = Map()

  def mkCFGs(): Unit = {
    // global field declarations
    globalTable.loadNodes(program.fields, location = AddrLocation.Data)

    // Create a CFG for each method
    for (method <- program.methods) {
      val cfg = MethodToCFG.convert(method, Some(globalTable))
      val name = method.id.token.string
      cfgs += (name -> cfg)
    }
  }

  def optCFGs(): Unit = {
    cfgs = cfgs.map {
      case (name, cfg) =>
        val newCFG = CFGOptimizer.opt(cfg)
        (name, newCFG)
      case _ =>
        throw new Exception(
          "In `CFGManager` invalid cfg for `optCFGs` (unreachable code)"
        )
    }
  }

  // NOTE we might want to capture these in a "transformations" abstraction
  def allocCFGs(): Unit = {
    GlobalVarAllocator.alloc(globalTable)
    cfgs = cfgs.map {
      case (name, cfg) =>
        val newCFG = CFGAllocator.alloc(name, cfg)
        (name, newCFG)
      case _ =>
        throw new Exception(
          "In `CFGManager` invalid cfg for `allocCFGs` (unreachable code)"
        )
    }
  }

  override def toString = {
    var str = ""
    for ((name, cfg) <- cfgs) {
      str += List(
        "***************",
        s"CFG for method `${name}`:",
        cfg.printed(),
        "***************"
      ).mkString("\n")
    }
    str
  }
}
