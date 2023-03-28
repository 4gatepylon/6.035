package edu.mit.compilers.codegen

// Here we have utility to find def-use chains for a single CFG

// Def and Use correspond to situations where you have something like
// x = expr(y, z, ...), which is a def of x and a use of y, z, ...
// (we use "line" to represent some notion of a line number so we
// can know which defs come before which uses)
trait DefOrUse {
  val reg: Register
  val line: Int
  val block: BasicBlock

  // NOTE: the "==" operators is used to check for exact equality using
  // the register!

  // < and <= are domination by reachability
  def <(that: DefOrUse): Boolean = {
    if (this.block != that.block) {
      this.block.reaches(that.block)
    } else {
      this.block.reaches(that.block) || this.line < that.line
    }
  }
  def <=(that: DefOrUse): Boolean = {
    if (this.block != that.block) {
      this.block.reaches(that.block)
    } else {
      this.block.reaches(that.block) || this.line <= that.line
    }
  }
  def >(that: DefOrUse): Boolean = !(this <= that)
  def >=(that: DefOrUse): Boolean = !(this < that)
}

case class Def(val reg: Register, val line: Int, val block: BasicBlock)
    extends DefOrUse {
  override def toString = s"Def $reg, line $line, block ${block.id}"
}
case class Use(val reg: Register, val line: Int, val block: BasicBlock)
    extends DefOrUse {
  override def toString = s"Use $reg, line $line, block ${block.id}"
}

case class Chain(val definition: Def, val usage: Use) {
  override def toString(): String = s"Chain(${definition} -> ${usage})"

  def reg(): Register = {
    assert(
      definition.reg == usage.reg,
      "definition and usage must have same register for a Chain"
    )
    definition.reg
  }

  def isValid: Boolean = {
    val sameReg: Boolean = definition.reg == usage.reg
    val ordered: Boolean =
      definition.block == usage.block ||
        definition.block.reaches(usage.block)
    sameReg && ordered
  }
}

case class Chainer(val cfg: BasicBlock) {
  // Constructor
  var defs: Map[BasicBlock, Set[Def]] = Map()
  var uses: Map[BasicBlock, Set[Use]] = Map()
  var populated: Boolean = false

  // Constructor
  assert(!populated)
  assert(
    cfg.functionHeader == Some(cfg),
    "Need to be given function header (CFG) to populate all def uses"
  )
  val blocks: List[BasicBlock] = cfg.lineage
  defs = blocks.map(bb => (bb, getBasicBlockDefs(bb))).toMap
  uses = blocks.map(bb => (bb, getBasicBlockUses(bb))).toMap
  populated = true
  // End Constructor

  def getBasicBlockDefs(bb: BasicBlock): Set[Def] = {
    val linedInstrs = bb.instrs.zipWithIndex

    val fxHeader: BasicBlock = bb.functionHeader.getOrElse(
      throw new Exception("Function header not populated in getBasicBlockDefs")
    )
    val rTable: RegTable = fxHeader.regTable
      .getOrElse(
        throw new Exception(
          "Register table (for local vars) not populated in getBasicBlockDefs"
        )
      )
      .parent
      .getOrElse(
        throw new Exception(
          "Register table (for params) not populated in getBasicBlockDefs"
        )
      )
    // There should be a global table (this is a sanity test)
    assert(rTable.parent.isDefined)
    val paramDefs =
      if (bb == fxHeader) rTable.paramRegs.map(r => Def(r, -1, bb)).toSet
      else Set()
    val instrDefs = linedInstrs.flatMap {
      case (instr: BasicInstr, line: Int) => {
        val defRegs: List[Register] = instr match {
          case instr: DestInstr => List(instr.dest)
          case _                => List()
        }
        assert(defRegs.forall(!_.isInstanceOf[ConstReg]))
        defRegs.map(reg => Def(reg, line, bb))
      }
    }.toSet
    paramDefs ++ instrDefs
  }

  def getBasicBlockUses(bb: BasicBlock): Set[Use] = {
    val linedInstrs = bb.instrs.zipWithIndex
    val forkUses = bb match {
      case forkBlock: ForkBlock => {
        forkBlock.condDest match {
          case Some(dest) => Set(Use(dest, bb.instrs.size, bb))
          case None       => Set()
        }
      }
      case _ => Set()
    }
    val instrUses = linedInstrs.flatMap {
      case (instr: BasicInstr, line: Int) => {
        val useRegs: List[Register] = (instr match {
          case instr: ArgsInstr => instr.argList
          case _                => List()
        }).filterNot(_.isInstanceOf[ConstReg]) ++ (instr match {
          case instr: DestInstr =>
            instr.dest match {
              case ArrElemAddr(base, index, location, id) => instr.dest.argList
              case _                                      => List()
            }
          case _ => List()
        }).filterNot(_.isInstanceOf[ConstReg])
        useRegs.map(reg => Use(reg, line, bb))
      }
    }.toSet
    forkUses ++ instrUses
  }

  // This gets you the def-use chains that you subsequently merge
  def getChains(): Set[Chain] = {
    assert(
      cfg.functionHeader == Some(cfg),
      "Need to be given function header (CFG) to get chains"
    )

    val parents: Set[BasicBlock] = cfg.lineage.toSet
    assert(populated)

    val chains = parents.flatMap(parent => {
      // If you have a back-edge then you should also include
      // def-uses that go backwards, otherwise you don't want to.
      // This catches when the parent is either a header or not.
      defs(parent).flatMap(definition => {
        val isShared = (du: DefOrUse) => du.reg == definition.reg
        val blockHasShared = (bb: BasicBlock) => defs(bb).exists(isShared)

        // defs and uses that share the definition register
        val sharedDefs = defs(parent).filter(isShared)
        val sharedUses = uses(parent).filter(isShared)

        // Find all valid uses for this def within parent block
        val afterDefLines = sharedDefs.map(_.line).filter(_ > definition.line)
        val nextDefLine = (afterDefLines + parent.instrs.size).min

        val validUses = sharedUses.filter(usage =>
          definition.line < usage.line && usage.line <= nextDefLine
        )

        // Find all valid uses for this def in child blocks
        val children =
          parent.children.flatMap(_.visit(identity, blockHasShared)).toSet

        val childValidUses = children.flatMap(child => {
          val sharedDefs = defs(child).filter(isShared)
          val sharedUses = uses(child).filter(isShared)

          val maxLine = child.instrs.size
          val nextDefLine = (sharedDefs.map(_.line) + maxLine).min

          val validUses = sharedUses.filter(_.line <= nextDefLine)
          validUses
        })

        (validUses ++ childValidUses).map(Chain(definition, _))
      })
    })
    chains
  }
}

case object Chainer {
  // Singleton for convenience
  def mk(bb: BasicBlock): Set[Chain] = {
    Chainer(bb).getChains()
  }
}
