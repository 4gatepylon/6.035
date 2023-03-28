package edu.mit.compilers.codegen

import edu.mit.compilers.parser._

object Destruct {
  // Visitor for destructing AST nodes into CFG blocks

  // Utility function for converting loop updates into assign statements
  def updateToAssign(update: ASTForUpdate) = {
    update match {
      case ASTAddForUpdate(token, loc, expr) =>
        ASTAddAssignStmt(token, loc, expr)
      case ASTSubForUpdate(token, loc, expr) =>
        ASTSubAssignStmt(token, loc, expr)
      case ASTIncrForUpdate(token, loc) => ASTIncrStmt(token, loc)
      case ASTDecrForUpdate(token, loc) => ASTDecrStmt(token, loc)
      case _ => throw new Exception(s"Invalid `for` update $update")
    }
  }

  // Visitor function which makes CFG sequential or forking blocks from AST nodes
  def tree(
      node: ASTNode,
      depth: Int,
      loop: Option[ForkBlock]
  ): (BasicBlock, SeqBlock) = {
    node match {
      case ASTBlock(token, fields, stmts) => {
        val newDepth = depth + 1
        // destruct field declarations
        val first = BlockMaker.seq(fields, None, newDepth, loop)
        var (begin: BasicBlock, end) = (first, first)
        // destruct statements
        var i = 0
        while (i < stmts.size) {
          val (nextBegin, nextEnd) = tree(stmts(i), newDepth, loop)
          BlockLinker.mk(end, nextBegin)
          begin = nextBegin
          end = nextEnd
          i = stmts(i) match {
            case b: ASTBreakStmt    => stmts.size
            case c: ASTContinueStmt => stmts.size
            case _                  => i + 1
          }
        }
        (first, end)
      }
      case condStmt: ASTCondStmt =>
        condStmt match {
          case ASTIfThenStmt(token, cond, thenBlock) => {
            val end = BlockMaker.noOp(None, depth, loop)
            val (beginThen, endThen) = tree(thenBlock, depth, loop)
            BlockLinker.mk(endThen, end)
            val begin =
              ShortCircuit.expr(cond, condStmt, beginThen, end, depth, loop)
            (begin, end)
          }
          case ASTIfElseStmt(token, cond, thenBlock, elseBlock) => {
            val end = BlockMaker.noOp(None, depth, loop)
            val (beginThen, endThen) = tree(thenBlock, depth, loop)
            val (beginElse, endElse) = tree(elseBlock, depth, loop)
            BlockLinker.mk(endThen, end)
            BlockLinker.mk(endElse, end)
            val begin =
              ShortCircuit.expr(
                cond,
                condStmt,
                beginThen,
                beginElse,
                depth,
                loop
              )
            (begin, end)
          }
          case ASTWhileStmt(token, cond, block) => {
            // create noOp as temporary trueChild of ForkBlock
            val noOp = BlockMaker.noOp(None, depth, loop)
            val end = BlockMaker.noOp(None, depth, loop)
            // must create begin ForkBlock first so block destruct can use it
            val begin =
              ShortCircuit.expr(cond, condStmt, noOp, end, depth, loop)
            val curLoop = Some(begin)
            val (beginBlock, endBlock) = tree(block, depth, curLoop)
            // NOTE: that shortcircuit sometimes has to introduce new BBs
            // so begin's children are not necessarily the noOp and end
            if (noOp.parents.size != 1) {
              throw new Exception(
                s"NoOp (final temporary trueChild) has ${noOp.parents.size} parents"
              )
            }
            BlockLinker.swap(noOp.parents.head, noOp, beginBlock)
            BlockLinker.mk(endBlock, begin)
            (begin, end)
          }
          case ASTForStmt(token, id, init, cond, update, block) => {
            // overview: convert for loop to while loop
            // append assign statement to block and convert to while loop
            val assign = updateToAssign(update)
            val newBlock = block.copy(stmts = block.stmts :+ assign)

            // create noOp as temporary trueChild of ForkBlock
            val noOp = BlockMaker.noOp(None, depth, loop)
            val end = BlockMaker.noOp(None, depth, loop)
            // must create begin ForkBlock first so block destruct can use it
            val begin =
              ShortCircuit.expr(cond, condStmt, noOp, end, depth, loop)
            val curLoop = Some(begin)
            val (beginBlock, endBlock) = tree(newBlock, depth, curLoop)
            // NOTE: that shortcircuit sometimes has to introduce new BBs
            // so begin's children are not necessarily the noOp and end
            if (noOp.parents.size != 1) {
              throw new Exception(
                s"NoOp (final temporary trueChild) has ${noOp.parents.size} parents"
              )
            }
            BlockLinker.swap(noOp.parents.head, noOp, beginBlock)
            BlockLinker.mk(endBlock, begin)

            // convert id = init and for update to assign statements
            val loc = ASTScalarLocation(id.token, id)
            val equal = ASTEqAssignStmt(AssOp("=", -1, -1), loc, init)
            val first = BlockMaker.seq(List(equal), Some(begin), depth, loop)
            (first, end)
          }
          case _ =>
            throw new Exception(s"Invalid conditional statement $condStmt")
        }
      case _ => {
        val block = BlockMaker.seq(List(node), None, depth, loop)
        (block, block)
      }
    }
  }

  // Utility function which modifies children of blocks that contain break or continue statements
  def reroute(begin: BasicBlock): Unit = {
    // assumes that merging happens later, so all blocks contain 0 or 1 statements
    for (block <- begin.lineage) {
      block match {
        // break or continue should only exist inside sequential blocks
        case s: SeqBlock => {
          if (s.nodes.size == 1) {
            s.nodes.head match {
              case b: ASTBreakStmt => {
                // redirect flow to end of encapsulating loop
                BlockLinker.rm(s, s.child.get)
                BlockLinker.mk(s, s.loop.get.falseChild)
              }
              case c: ASTContinueStmt => {
                // redirect flow to beginning of encapsulating loop
                s.loop.get.condStmt match {
                  case f: ASTForStmt => {
                    val assign = updateToAssign(f.update)
                    s.nodes = assign :: s.nodes
                    BlockLinker.rm(s, s.child.get)
                    BlockLinker.mk(s, s.loop.get)
                  }
                  case w: ASTWhileStmt => {
                    BlockLinker.rm(s, s.child.get)
                    BlockLinker.mk(s, s.loop.get)
                  }
                  case _ =>
                    throw new Exception(
                      s"Loop cannot have conditional statement ${s.loop.get.condStmt}"
                    )
                }
              }
              case _ =>
            }
          }
        }
        case _ =>
      }
    }
  }
}
