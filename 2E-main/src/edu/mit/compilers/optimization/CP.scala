package edu.mit.compilers.optimization

import edu.mit.compilers.codegen._

// Copy Propagation
case object CP extends Optimization {
  type LatticeElement = List[Tuple2[Register, Register]]

  // CP: A map from tmps/vars to the tmps/vars that they are assigned to
  var ins = Map[BasicBlock, LatticeElement]()
  var outs = Map[BasicBlock, LatticeElement]()

  var bot: LatticeElement = List()
  val I: LatticeElement = List()

  def transferInstr(instr: BasicInstr, in: LatticeElement): LatticeElement =
    instr match {
      case instr: DestInstr => {
        var out = in
        out = out.filter(_._1 != instr.dest)
        out = out.filter(_._2 != instr.dest)

        instr match {
          case CallInstr(name, dest, args) => {
            // filter out global variables that could be mutated in function calls
            out = out.filter {
              case (dst: ScalarAddr, src: ScalarAddr) =>
                dst.location == AddrLocation.Stack && src.location == AddrLocation.Stack
              case (dst: ScalarAddr, src: IntConstReg) =>
                dst.location == AddrLocation.Stack
              case _ => false
            }
            out
          }
          case CopyInstr(dest, arg) => {
            (dest, arg) match {
              // skip adding (dest, dest) to avoid infinite loop
              case (_: ScalarAddr, _: ScalarAddr) if dest != arg =>
                out :+= (dest, arg)
              case (_: ScalarAddr, _: IntConstReg) =>
                out :+= (dest, arg)
              case _ =>
            }
            out
          }
          case _ => out
        }
      }
      case _ => in
    }

  def transferBlock(bb: BasicBlock, in: LatticeElement): LatticeElement = {
    bb.instrs.foldLeft(in) {
      case (cur, instr) => transferInstr(instr, cur)
      case _            => throw new Exception("invalid key value pair")
    }
  }

  def join(outs: List[LatticeElement]): LatticeElement = {
    outs.foldLeft(bot)(_.intersect(_))
  }

  def cp(bb: BasicBlock): Unit = {
    var cur = ins(bb)

    // get equivalent register that is furthest ancestor
    // creates infinite recursion if there exist cycles
    def origReg(reg: Register): Register = {
      val idx = cur.indexWhere(_._1 == reg)
      if (idx >= 0) origReg(cur(idx)._2) else reg
    }

    bb.instrs = bb.instrs.map(instr => {
      val newInstr = instr match {
        case instr: DestInstr => {
          instr match {
            case binOp: BinOpInstr => {
              binOp.arg1 = origReg(binOp.arg1)
              binOp.arg2 = origReg(binOp.arg2)
              binOp
            }
            case unOp: UnOpInstr => {
              unOp.arg = origReg(unOp.arg)
              unOp
            }
            case CallInstr(name, dest, args) => {
              // get original registers before mutating cur
              val origArgs = args.map(origReg)
              CallInstr(name, dest, origArgs)
            }
            case CopyInstr(dest, arg) => {
              CopyInstr(dest, origReg(arg))
            }
          }
        }
        case RetInstr(arg)     => RetInstr(origReg(arg))
        case instr: BasicInstr => instr
      }
      cur = transferInstr(newInstr, cur)
      newInstr
    })
  }

  override def opt(cfg: BasicBlock): BasicBlock = {
    assert(
      cfg.functionHeader.getOrElse(
        throw new Exception("Provide function header to `CP.opt`")
      ) == cfg,
      "Need to be given function header (CFG) to `CP.opt`"
    )

    // TODO: pass cfg as argument in object constructor
    // initialize bottom lattice element
    bot = cfg.lineage
      .flatMap(_.instrs)
      .filter {
        case CopyInstr(dest, arg) =>
          (dest, arg) match {
            case (_: ScalarAddr, _: ScalarAddr)  => dest != arg
            case (_: ScalarAddr, _: IntConstReg) => true
            case _                               => false
          }
        case _ => false
      }
      .map {
        case CopyInstr(dest, arg) => (dest, arg)
        case _ => throw new Exception("invalid copy instruction")
      }
    // initialize default in and out for each basic block
    cfg.lineage.foreach(bb => {
      ins += (bb -> bot)
      outs += (bb -> transferBlock(bb, bot))
    })

    // initialize in and out for starting block
    ins += (cfg -> I)
    outs += (cfg -> transferBlock(cfg, I))

    // worklist fixed point algorithm
    var worklist = cfg.lineage.toSet - cfg
    while (worklist.nonEmpty) {
      val bb = worklist.head
      worklist -= bb

      val in = join(bb.parents.map(outs(_)))
      ins += (bb -> in)

      val out = transferBlock(bb, in)
      if (out != outs(bb)) {
        outs += (bb -> out)
        worklist ++= bb.children
      }
    }

    cfg.visit(cp)
    cfg
  }

  override def toString(): String = "cp"

  override def precedence(): Int = Precedences.CP
}
