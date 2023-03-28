package edu.mit.compilers.codegen

import edu.mit.compilers.parser._
import edu.mit.compilers.optimization.LoopDepth

trait BasicBlock {
  // Id is used to help generate jumps in code-gen
  val id: Int
  // list of nodes in this block
  var nodes: List[ASTNode]
  // list of corresponding instructions
  var instrs: List[BasicInstr] = List()
  // We may or may not support lists of parents and children later...
  var parents: List[BasicBlock] = List()
  var children: List[BasicBlock]
  // depth of scope nesting, whether the method this is part of is typed,
  // and table mapping variable names to registers so we can reuse variables
  val scopeDepth: Int
  var methodIsTyped: Boolean = false
  var regTable: Option[RegTable] = None
  // ForkBlock header of the loop that encapsulates this block
  // (header should not have this)
  val loop: Option[ForkBlock]
  // To know what parameters you have (for def-use for example)
  // you can use the function header's function params
  var functionHeader: Option[BasicBlock] = None
  // This makes regAlloc pog
  var numExcessParams = 0
  var maxColor = -1

  override def toString: String = {
    val sep = "  "
    val parentIdStr = ("Parent IDs:" :: parents.map(_.id)).mkString(s"\n$sep")
    val idStr = s"This ID:\n$sep$id"
    val childIdStr = ("Child IDs:" :: children.map(_.id)).mkString(s"\n$sep")
    val nodeStr = ("Nodes:" :: nodes).mkString(s"\n$sep")
    val instrStr = ("Instrs:" :: instrs).mkString(s"\n$sep")
    val loopIdStr = s"Loop ID:\n$sep${loop.map(_.id).getOrElse("None")}"
    val scopeDepthStr = s"Scope depth:\n$sep$scopeDepth"
    val regTableStr = s"RegTable:\n${regTable.getOrElse("None")}"
    val loopDepthStr = s"Loop depth:\n${LoopDepth.getLoopDepth(this)}"
    List(
      parentIdStr,
      idStr,
      childIdStr,
      nodeStr,
      instrStr,
      loopIdStr,
      scopeDepthStr,
      regTableStr,
      loopDepthStr
    ).mkString(
      "\n"
    )
  }

  override def equals(other: Any): Boolean = other match {
    case that: BasicBlock => this.id == that.id
    case _                => false
  }

  override def hashCode: Int = id

  // visit all reachable blocks and apply the given function to each
  def visit[T](
      map: (BasicBlock => T),
      // condition to stop traversal beyond this block
      stop: (BasicBlock => Boolean) = _ => false
  ): List[T] = {
    var contour = Set[BasicBlock](this)
    var visited = Set[BasicBlock]()
    var result = List[T]()
    while (contour.size > 0) {
      result = result ++ contour.map(map)
      visited = visited ++ contour
      contour = contour.filterNot(stop).flatMap(_.children).filterNot(visited)
    }
    result
  }

  // return all blocks that are reachable from this block
  def lineage(): List[BasicBlock] = visit(identity)

  def reaches(child: BasicBlock): Boolean = {
    // A block can only reach itself if its children can reach it
    child == this match {
      case true =>
        this.children.contains(this) || children
          .filterNot(_ == this)
          .foldLeft(false)((acc, child) => acc || child.reaches(this))
      case false => lineage.contains(child)
    }
  }

  def printed(): String = {
    val sb = new StringBuilder()
    val strings: List[String] = visit[String](_.toString)
    sb.append(s"CFG has ${strings.size} blocks")
    for (str <- strings) {
      sb.append("\n******\n")
      sb.append(str)
      sb.append("\n******\n")
    }
    sb.toString()
  }

  def addParent(parent: BasicBlock): Unit = {
    parents :+= parent
  }

  def rmParent(parent: BasicBlock): Unit = {
    parents = parents.filter(_ != parent)
  }

  // swap oldChild with newChild (if oldChild is not a child, throw an exception)
  def swapChild(oldChild: BasicBlock, newChild: BasicBlock): Unit
}

// Here we can list out our different types of basic blocks
case class SeqBlock(
    val id: Int,
    // Forks are split points
    var nodes: List[ASTNode],
    var child: Option[BasicBlock],
    val scopeDepth: Int,
    val loop: Option[ForkBlock]
) extends BasicBlock {
  var children: List[BasicBlock] =
    if (child.isEmpty) List() else List(child.get)

  def setChild(newChild: BasicBlock): Unit = {
    child = Some(newChild)
    children = List(newChild)
  }

  def popChild(): Option[BasicBlock] = {
    val oldChild = child
    child = None
    children = List()
    oldChild
  }

  def swapChild(oldChild: BasicBlock, newChild: BasicBlock): Unit = {
    if (child.exists(_ == oldChild)) {
      child = Some(newChild)
      children = List(newChild)
    } else {
      throw new Exception(
        s"SeqBlock.swapChild: oldChild ${oldChild.id} not found, instead had ${child.getOrElse("None")}"
      )
    }
  }
}

case class ForkBlock(
    val id: Int,
    // Forks are split points
    var cond: ASTExpr,
    // cond is a subexpr of condStmt.cond
    val condStmt: ASTCondStmt,
    var trueChild: BasicBlock,
    var falseChild: BasicBlock,
    val scopeDepth: Int,
    val loop: Option[ForkBlock],
    // This helps us deal with singletons like if (true) as opposed to if(true == true)
    var condDest: Option[Register] = None
) extends BasicBlock {
  var children: List[BasicBlock] = List(trueChild, falseChild)
  var nodes: List[ASTNode] = List(cond)

  def setTrueChild(newChild: BasicBlock): Unit = {
    trueChild = newChild
    children = List(trueChild, falseChild)
  }

  def setFalseChild(newChild: BasicBlock): Unit = {
    falseChild = newChild
    children = List(trueChild, falseChild)
  }

  def swapChild(oldChild: BasicBlock, newChild: BasicBlock): Unit = {
    if (trueChild == oldChild) {
      trueChild = newChild
    } else if (falseChild == oldChild) {
      falseChild = newChild
    } else {
      throw new Exception(
        s"ForkBlock.swapChild: oldChild ${oldChild.id} not found, instead have true=${trueChild.id} and false=${falseChild.id}"
      )
    }
    children = List(trueChild, falseChild)
  }
}

object BlockMaker {
  // NOTE that this is a static and will change throughout the lifecycle of the program
  var curId = 0

  // Canonical Block creators (important because
  // the CFG converter has a latestBlockId which is used to provide valid
  // unique ids to the blocks so as to enable easy jump code later)
  def noOp(
      child: Option[BasicBlock],
      scopeDepth: Int,
      // Loop stores the loop header of the loop nearest up in scope
      loop: Option[ForkBlock]
  ): SeqBlock = seq(List(), child, scopeDepth, loop)

  def fork(
      cond: ASTExpr,
      condStmt: ASTCondStmt,
      trueChild: BasicBlock,
      falseChild: BasicBlock,
      scopeDepth: Int,
      loop: Option[ForkBlock]
  ): ForkBlock = {
    val fork = ForkBlock(
      curId,
      cond,
      condStmt,
      trueChild,
      falseChild,
      scopeDepth,
      loop
    )
    BlockLinker.mk(fork, trueChild, falseChild)
    curId += 1
    fork
  }

  def seq(
      nodes: List[ASTNode],
      child: Option[BasicBlock],
      scopeDepth: Int,
      loop: Option[ForkBlock]
  ): SeqBlock = {
    val seq = SeqBlock(curId, nodes, child, scopeDepth, loop)
    if (child.isDefined) {
      BlockLinker.mk(seq, child.get)
    }
    curId += 1
    seq
  }
}

object BlockLinker {
  // mk links parent and child blocks
  def mk(parent: SeqBlock, child: BasicBlock): Unit = {
    parent.setChild(child)
    child.addParent(parent)
  }

  def mk(
      parent: ForkBlock,
      trueChild: BasicBlock,
      falseChild: BasicBlock
  ): Unit = {
    mkTrue(parent, trueChild)
    mkFalse(parent, falseChild)
  }

  def mkTrue(parent: ForkBlock, child: BasicBlock): Unit = {
    parent.setTrueChild(child)
    child.addParent(parent)
  }

  def mkFalse(parent: ForkBlock, child: BasicBlock): Unit = {
    parent.setFalseChild(child)
    child.addParent(parent)
  }

  // rm unlinks parent and child blocks
  def rm(parent: SeqBlock, child: BasicBlock): Unit = {
    parent.popChild()
    child.rmParent(parent)
  }

  // swap replaces the old child of a parent with the new child
  def swap(
      parent: BasicBlock,
      oldChild: BasicBlock,
      newChild: BasicBlock
  ): Unit = {
    parent.swapChild(oldChild, newChild)
    oldChild.rmParent(parent)
    newChild.addParent(parent)
  }
}

object BlockMerger {
  // merge parent into child and return deleted parent
  def fuse(parent: SeqBlock, child: BasicBlock): SeqBlock = {
    // could merge child into parent, but then we must update the parent's class
    assert(parent.child.isDefined, "parent must have exactly 1 child")
    assert(child.parents.size == 1, "child must have exactly 1 parent")
    assert(
      parent.scopeDepth == child.scopeDepth,
      "blocks to fuse must have same scope depth"
    )

    child.nodes = parent.nodes ++ child.nodes
    parent.nodes = List()
    BlockLinker.rm(parent, child)
    for (gp <- parent.parents) {
      BlockLinker.swap(gp, parent, child)
    }
    parent
  }

  // TODO: make this mutate a copy and return it (without mutating the alias)?
  def all(block: BasicBlock): BasicBlock = {
    // single pass over blocks to merge at most 1 chained pair of blocks
    def once(begin: BasicBlock): (BasicBlock, Boolean) = {
      for (b <- begin.lineage) {
        b match {
          // parent must be SeqBlock
          case parent: SeqBlock => {
            // merge if parent has 1 child and child has 1 parent
            if (parent.child.exists(_.parents.size == 1)) {
              val child = parent.child.get
              val removed = fuse(parent, child)
              // completed merge, return updated begin block
              return (if (begin == removed) child else begin, true)
            }
          }
          case _ => // continue
        }
      }
      // no merges made in this pass, so return false
      return (begin, false)
    }

    // loop until all merges are made
    var begin = block
    var changed = true
    while (changed) {
      val (newBegin, newChanged) = once(begin)
      begin = newBegin
      changed = newChanged
    }
    begin
  }
}

object RegTableInserter {
  def once(cur: BasicBlock, prev: BasicBlock): Unit = {
    (cur.scopeDepth - prev.scopeDepth) match {
      // Pop
      case -1 => {
        // In the future we could pop from the stack here, optionally
        val prevTable =
          prev.regTable.getOrElse(throw new Exception("prev must have table"))
        cur.regTable = prevTable.parent
      }
      // Do Nothing
      case 0 => {
        cur.regTable = prev.regTable
      }
      // Push
      case 1 => {
        // Make a new table and fill it with all the fields in this basic block
        val nextTable = RegTable(prev.regTable)
        val declInstrs: List[BasicInstr] = nextTable.loadNodes(cur.nodes)
        cur.instrs = cur.instrs ++ declInstrs
        cur.regTable = Some(nextTable)
      }
      // Impossible Case
      case _ => {
        throw new Exception(
          "Must grow or lower table scope depth by 1, 0, or -1"
        )
      }
    }
  }

  // When you call .all you must pass in the PREVIOUS table (expecting the push
  // code to detect a push due to a change in scope depth and then load new fields into
  // a new table)
  def all(start: BasicBlock, prevTable: Option[RegTable]): Unit = {
    // Dummy used exclusively for its depth and table
    val prev = BlockMaker.noOp(None, start.scopeDepth - 1, None)
    prev.regTable = prevTable

    // Store who to visit and what scope depth and parent register table they are being visited from
    var contour = Set[BasicBlock](start)
    var curToPrev = Map[BasicBlock, BasicBlock](start -> prev)

    var visited = Set[BasicBlock]()
    while (contour.size > 0) {
      // insert register tables into blocks
      contour.foreach(bb => once(bb, curToPrev(bb)))

      visited = visited ++ contour

      curToPrev = contour.flatMap(bb => bb.children.map(c => (c, bb))).toMap
      contour = contour.flatMap(bb => bb.children) -- visited
    }
  }
}
