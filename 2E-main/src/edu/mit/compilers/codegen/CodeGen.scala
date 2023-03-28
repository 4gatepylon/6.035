package edu.mit.compilers.codegen

import edu.mit.compilers.parser._

trait BaseGenerator {
  val callerSaved = RegLocInfo.paramRegLocs.reverse.map(_.toString)
  val calleeSaved = List("%r12", "%r13", "%r14", "%r15", "%rbx")

  // Helper to insert the appropriate spaces, tabs, etcetera
  def fmt(cmd: String, args: String*): String = {
    val padSize = 8
    val cmdStr = cmd.padTo(padSize, " ").mkString
    val argsStr = args.mkString(", ")
    s"    $cmdStr$argsStr\n"
  }

  // Because main has to initialize globals, we need to skip that part of main
  // for which we use a label: `Header.main`
  def callName(name: String): String = if (name == "main") Header.main else name

  // Headers and header prefixes (i.e. places you jump to
  // or labels for globals)
  object Header {
    val block = ".basicBlock"
    val data = ".data"
    val string = ".string"
    val failedIndex = ".failedIndex"
    val failedReturn = ".failedReturn"
    val main = ".main"
    val regAlloc: Boolean = false
  }

  // Blocks included in all generated code
  object KeyBlocks {
    // Blocks that are commonly generated for failure cases
    def failedBlock(header: String, returnCode: Int): String = {
      val sb = new StringBuilder()
      sb ++= s"${header}:\n"
      sb ++= fmt("# set %rax to our desired syscall (1 for exit)")
      sb ++= fmt("movq", "$1", "%rax")
      sb ++= fmt("# set %rbx to our desired syscall's argument (1 for nonzero)")
      sb ++= fmt("movq", s"$$${returnCode}", "%rbx")
      sb ++= fmt("# interrupt, deferring control to the OS")
      sb ++= fmt("int", "$0x80")
      sb.toString()
    }
    def failedIndexBlock(): String = failedBlock(Header.failedIndex, -1)
    def failedReturnBlock(): String = failedBlock(Header.failedReturn, -2)

    // Block that is emitted at the top of the file to allocate space for globals
    def globalBlock(): String = {
      val sb = new StringBuilder()

      // Data includes all strings, global arrays, and global scalars
      // String format is used here to deal with decaf cases where printf
      // is not given a format string (which is always expected by printf).
      sb ++= ".data\n"
      sb ++= "strFormat:\n"
      sb ++= fmt(".string", """"%s"""")
      sb ++= fmt(".align", "16")
      sb ++= "intFormat:\n"
      sb ++= fmt(".string", """"%d"""")
      sb ++= fmt(".align", "16")
      for ((string, label) <- RegAllocator.stringToLabel) {
        sb ++= s"$label:\n"
        sb ++= fmt(".string", s""""$string"""")
        sb ++= fmt(".align", "16")
      }
      for ((name, label) <- RegAllocator.dataVarNameToLabel) {
        // getOrElse(0) is a little bit of a hack for scalars, might want to add a case
        val length: Long = label.length.getOrElse(0L)
        sb ++= s"$label:\n"
        sb ++= fmt(".zero", s"${(length + 1L) * RegAllocator.stackStride}")
        sb ++= fmt(".align", "16")
      }
      sb ++= ".text\n"
      sb ++= ".globl main\n"
      sb.toString()
    }

    def globalInitBlock(): String = {
      val sb = new StringBuilder()
      sb ++= "# initialization of globals (only called in the beginning)\n"
      for ((name, label) <- RegAllocator.dataVarNameToLabel) {
        // move array length into array header
        label.length match {
          case Some(length) => sb ++= fmt("movq", s"$$$length", s"$label")
          case None         =>
        }
      }
      sb ++= s"${Header.main}:\n"
      sb.toString()
    }
  }
}

trait ProgramGenerator extends BaseGenerator {
  // remove duplicate jumps to label on next line
  def rmDuplJmps(code: String): String = {
    val lines = code.split("\n")

    if (lines.length <= 1) return code

    val sb = new StringBuilder()
    sb ++= lines(0) + "\n"
    for (i <- 1 until lines.length) {
      val (jump, header) = (lines(i - 1), lines(i))
      if (jump.contains("jmp") && header.contains(Header.block)) {
        val start = header.indexOf(Header.block)
        assert(start >= 0)
        val end = header.indexOf(":", start)
        val blockName =
          header.substring(start, if (end >= 0) end else header.length)
        if (!jump.contains(blockName)) {
          sb ++= jump + "\n"
        } else {
          // comment out redundant jump
          sb ++= "#\t" + jump + "\n"
        }
      } else {
        sb ++= jump + "\n"
      }
    }
    sb.toString
  }

  def emit(manager: CFGManager): String = {
    // A little sus, but this should do the trick to get us
    // a unique label per string

    val sb = new StringBuilder()
    sb ++= KeyBlocks.globalBlock()
    var nameToNumRegs = Map[String, Int]("main" -> 0)
    val definedFuncs = manager.cfgs.keySet
    manager.cfgs.map({
      case (name, cfg) => {
        cfg.lineage.map(bb => {
          bb.instrs.map(instr =>
            instr match {
              case call: CallInstr => {
                val numRegsUsed = call.args.length
                nameToNumRegs += (call.funcName -> numRegsUsed)
              }
              case _ =>
            }
          )
        })
      }
    })
    for ((name, cfg) <- manager.cfgs) {
      val maxParamsUsed =
        cfg.lineage.foldLeft[Int](0)((acc: Int, cur: BasicBlock) =>
          acc.max(
            if (cur.instrs.length == 0) 0
            else
              cur.instrs
                .map(instr =>
                  instr match {
                    case call: CallInstr =>
                      nameToNumRegs(call.funcName)
                    case _ => 0
                  }
                )
                .max
          )
        )
      // a param reg is unused within a method if the method doesn't use it
      // AND the methods it DIRECTLY calls don't use it
      val numExcessParams =
        RegLocInfo.paramRegLocs
          .map(_.toString)
          .drop(nameToNumRegs(name).max(maxParamsUsed))
          .length
      cfg.numExcessParams = numExcessParams
      sb ++= emitMethod(name, cfg)
    }

    // This is a block you can jump to if you ever fail indices
    sb ++= KeyBlocks.failedIndexBlock()
    // This is a block you can jump to if you ever fail to return
    sb ++= KeyBlocks.failedReturnBlock()

    rmDuplJmps(sb.toString())
  }

  def emitMethod(name: String, cfg: BasicBlock): String = {
    // see slide 81 of 182 for lecture 7 on unoptimized codegen
    val sb = new StringBuilder()

    // Add jump label
    sb ++= s"$name:\n"

    if (name == "main") {
      sb ++= KeyBlocks.globalInitBlock()
    }
    sb ++= fmt("# prologue ritual")
    sb ++= fmt("pushq", "%rbp")
    sb ++= fmt("movq", "%rsp", "%rbp")
    sb ++= fmt("# allocate temp space")
    sb ++= fmt("addq", s"$$${CFGAllocator.MaxRspTable.get(name)}", "%rsp")

    // The last block will have a return
    sb ++= emitBlocks(cfg, name)
    sb.toString()
  }

  def emitBlocks(cfg: BasicBlock, name: String): String = {
    // consider a topological sort for the blocks
    // begin block first, end block last
    val blockGen = BlockGenerator(cfg, name)
    // These have to be run after BlockGenerator does its thing
    val sb = new StringBuilder()
    for (reg <- callerSaved.take(cfg.numExcessParams.min(cfg.maxColor + 1))) {
      sb ++= fmt("xorq", reg, reg)
    }
    for (reg <- calleeSaved.take(1 + cfg.maxColor - cfg.numExcessParams)) {
      sb ++= fmt("pushq", reg)
      sb ++= fmt("xorq", reg, reg)
    }
    if (
      calleeSaved.take(1 + cfg.maxColor - cfg.numExcessParams).length % 2 == 1
    ) sb ++= fmt("pushq", "$0")
    val codeBlocks = cfg.visit[String](blockGen.emitBlock(_, name))
    sb.mkString("") ++ codeBlocks.mkString("")
  }
}

case class BlockGenerator(cfg: BasicBlock, name: String)
    extends ProgramGenerator {
  assert(cfg.functionHeader.isDefined, "Function header is not defined")
  val chains = Chainer.mk(cfg.functionHeader.get)
  val webs = Webber.mergeChains(chains)
  val graph = WebGrapher.makeGraph(webs)
  val colors =
    WebColorer.colorWebs(graph, cfg.numExcessParams + calleeSaved.length)
  val regToColor = WebFinder.defOrUseToWeb(webs)
  cfg.maxColor = if (colors.size > 0) colors.values.max else -1

  // This function returns the thing you refer to it as (i.e. "-8(%rbp)", "%rdi", "$0")
  // AND any instructions that need to be run before you can make that reference (like leaq)
  def regLocation(
      reg: Register,
      bb: BasicBlock,
      line: Int,
      isDef: Boolean,
      ignoreRegAlloc: Boolean = false
  ): (String, String) =
    reg match {
      case n: Addr => {
        val defaultLoc: (String, String) = n.location match {
          case AddrLocation.Data => {
            val offset = RegAllocator.getLoc(n)
            offset match {
              case o: DataLabel => {
                n match {
                  case _ @(_: ArrBaseAddr | _: ScalarAddr) => (s"$o", "")
                  case a =>
                    throw new Exception(s"This addr shouldn't be in data: $a")
                }
              }
              case o: StringLabel => {
                n match {
                  case _: StrAddr => (s"$o", "")
                  case a =>
                    throw new Exception(s"This addr shouldn't be in data: $a")
                }
              }
              case o: DynamicOffset => regLocation(o.src, bb, line, false)
              case o: ArrAddrOffset => {
                val sb = new StringBuilder()
                sb ++= (o.index match {
                  case d: DynamicOffset => {
                    val (ind, instrs) = regLocation(d.src, bb, line, false)
                    sb ++= instrs
                    fmt("movq", ind, "%r11")
                  }
                  case e =>
                    throw new Exception(
                      s"Unsupported Offset type: $e for `ArrAddrOffset` index"
                    )
                })
                sb ++= fmt(
                  "leaq",
                  s"${Header.data}${o.base.value} + ${RegAllocator.stackStride}",
                  "%r10"
                )
                (s"(%r10, %r11, ${RegAllocator.stackStride})", sb.toString())
              }
              case a =>
                throw new Exception(s"Unsupported Offset type: $a for Data")
            }
          }
          case AddrLocation.Stack => {
            val offset = RegAllocator.getLoc(n)
            offset match {
              case o: StaticOffset  => (s"${o.value}(%rbp)", "")
              case o: DynamicOffset => regLocation(o.src, bb, line, false)
              case o: ArrAddrOffset => {
                val sb = new StringBuilder()
                sb ++= (o.index match {
                  case d: DynamicOffset => {
                    val (ind, instrs) = regLocation(d.src, bb, line, false)
                    sb ++= instrs
                    fmt("movq", ind, "%r11")
                  }
                  case e =>
                    throw new Exception(
                      s"Unsupported  Offset type: $e for `ArrAddrOffset` index"
                    )
                })
                sb ++= fmt(
                  "leaq",
                  s"${o.base.value + RegAllocator.stackStride}(%rbp)",
                  "%r10"
                )
                (s"(%r10, %r11, ${RegAllocator.stackStride})", sb.toString())
              }
              case a =>
                throw new Exception(s"Unsupported Offset type: $a for Stack")
            }
          }
          case addrLocation =>
            throw new Exception(s"Unknown AddrLocation $addrLocation")
        }
        if (CodeGenerator.regAlloc && !ignoreRegAlloc) {
          val defOrUse =
            if (isDef) Def(reg, line, bb)
            else Use(reg, line, bb)
          if (
            regToColor.contains(defOrUse) && colors.contains(
              regToColor(defOrUse)
            ) && colors(regToColor(defOrUse)) >= 0
          ) {
            val color = colors(regToColor(defOrUse))
            val allocReg =
              if (color >= bb.functionHeader.get.numExcessParams)
                calleeSaved(color - bb.functionHeader.get.numExcessParams)
              else callerSaved(color)
            (allocReg, "")
          } else defaultLoc
        } else defaultLoc
      }
      case n: IntConstReg  => (s"$$${n.value}", "")
      case c: CharConstReg => (s"$$${c.value.toInt}", "")
      case b: BoolConstReg => (if (b.value) "$1" else "$0", "")
      case r => {
        if (RegLocInfo.isBuiltIn(r)) {
          (r.toString, "")
        } else {
          throw new Exception(s"Register does not have a location: $r")
        }
      }
    }

  // hack to pass previous instruction to all relevant methods without bloating arguments
  var prevInstr: Option[BasicInstr] = None

  def emitBlock(block: BasicBlock, name: String): String = {
    val sb = new StringBuilder()
    sb ++= s"${Header.block}${block.id}:\n"
    // TODO: initialize previous instruction to be last instruction of parent block if unique
    prevInstr = None
    for ((instr, line) <- block.instrs.zipWithIndex) {
      sb ++= emitInstr(instr, line, block, name)
      prevInstr = Some(instr)
    }
    block match {
      case seq: SeqBlock => {
        if (seq.child.isDefined) {
          sb ++= fmt("jmp", s"${Header.block}${seq.child.get.id}")
        } else {
          if (block.methodIsTyped) {
            // You should have returned by this point, if you make it here you fail
            sb ++= fmt("jmp", Header.failedReturn)
          } else {
            sb ++= fmt("# epilogue ritual")
            if (
              calleeSaved
                .take(
                  1 + block.functionHeader.get.maxColor - block.functionHeader.get.numExcessParams
                )
                .length % 2 == 1
            )
              sb ++= fmt(
                "add",
                s"$$${RegAllocator.stackStride}",
                "%rsp"
              )
            for (
              reg <- calleeSaved
                .take(
                  1 + block.functionHeader.get.maxColor - block.functionHeader.get.numExcessParams
                )
                .reverse
            ) {
              sb ++= fmt("popq", reg)
            }
            sb ++= fmt(
              "subq",
              s"$$${CFGAllocator.MaxRspTable.get(name)}",
              "%rsp"
            )
            sb ++= fmt("leave")
            // All voids (including main) will return zero in %rax
            sb ++= fmt("movq", "$0", "%rax")
            sb ++= fmt("ret")
          }
        }
      }
      case fork: ForkBlock => {
        // assumes the last instruction has a register storing the boolean result
        val dest = fork.condDest.getOrElse(
          throw new Exception(
            s"Fork ${fork.id} has no destination for the conditional value"
          )
        )
        sb ++= emitSubInstr(
          "movq",
          dest,
          Some(RegMaker.tmpBuiltInReg(RaxLoc)),
          fork,
          fork.instrs.length
        )
        sb ++= fmt("cmp", "$0", "%rax")
        sb ++= fmt("je", s"${Header.block}${fork.falseChild.id}")
        sb ++= fmt("jmp", s"${Header.block}${fork.trueChild.id}")
      }
      case _ => throw new Exception(s"unexpected block type ${block}")
    }
    sb.toString()
  }

  def emitInstr(
      instr: BasicInstr,
      line: Int,
      bb: BasicBlock,
      name: String
  ): String = {
    val sb = new StringBuilder()
    sb ++= fmt(s"# $line: $instr")
    instr match {
      case binOpInstr: BinOpInstr => {
        val opStr = binOpInstr.arg1 match {
          case StrFormat  => "leaq"
          case _: StrAddr => "leaq"
          case b: ArrBaseAddr => {
            b.location match {
              case AddrLocation.Data  => "leaq"
              case AddrLocation.Stack => "movq"
              case _ =>
                throw new Exception(s"Unknown AddrLocation ${b.location}")
            }
          }
          case _ => "movq"
        }
        sb ++= emitSubInstr(
          opStr,
          binOpInstr.arg1,
          Some(RegMaker.tmpBuiltInReg(RaxLoc)),
          bb,
          line
        )
        binOpInstr match {
          case arithInstr: ArithInstr => {
            arithInstr match {
              case AddInstr(dest, arg1, arg2) => {
                sb ++= emitSubInstr(
                  "addq",
                  arg2,
                  Some(RegMaker.tmpBuiltInReg(RaxLoc)),
                  bb,
                  line
                )
              }
              case SubInstr(dest, arg1, arg2) => {
                sb ++= emitSubInstr(
                  "subq",
                  arg2,
                  Some(RegMaker.tmpBuiltInReg(RaxLoc)),
                  bb,
                  line
                )
              }
              case MulInstr(dest, arg1, arg2) => {
                sb ++= emitSubInstr(
                  "imulq",
                  arg2,
                  Some(RegMaker.tmpBuiltInReg(RaxLoc)),
                  bb,
                  line
                )
              }
              case DivInstr(dest, arg1, arg2) => {
                val rcxUsed = regLocation(arg2, bb, line, false)._1 == "%rcx"
                val rdxUsed = regLocation(arg2, bb, line, false)._1 == "%rdx"
                val (arg2StackLoc, arg2StackInstrs) =
                  regLocation(arg2, bb, line, false, true)
                if (rdxUsed) {
                  sb ++= arg2StackInstrs
                  sb ++= fmt("movq", "%rdx", arg2StackLoc)
                } else sb ++= fmt("pushq", "%rdx")
                if (rcxUsed) {
                  sb ++= arg2StackInstrs
                  sb ++= fmt("movq", "%rcx", arg2StackLoc)
                } else sb ++= fmt("pushq", "%rcx")
                sb ++= fmt("cqo")
                if (rcxUsed || rdxUsed) {
                  sb ++= fmt("movq", arg2StackLoc, "%rcx")
                } else {
                  sb ++= emitSubInstr(
                    "movq",
                    arg2,
                    Some(RegMaker.tmpBuiltInReg(RcxLoc)),
                    bb,
                    line
                  )
                }
                sb ++= fmt("idivq", "%rcx")
                if (rcxUsed) {
                  sb ++= arg2StackInstrs
                  sb ++= fmt("movq", arg2StackLoc, "%rcx")
                } else sb ++= fmt("popq", "%rcx")
                if (rdxUsed) {
                  sb ++= arg2StackInstrs
                  sb ++= fmt("movq", arg2StackLoc, "%rdx")
                } else sb ++= fmt("popq", "%rdx")
              }
              case ModInstr(dest, arg1, arg2) => {
                val rcxUsed = regLocation(arg2, bb, line, false)._1 == "%rcx"
                val rdxUsed = regLocation(arg2, bb, line, false)._1 == "%rdx"
                val (arg2StackLoc, arg2StackInstrs) =
                  regLocation(arg2, bb, line, false, true)
                if (rdxUsed) {
                  sb ++= arg2StackInstrs
                  sb ++= fmt("movq", "%rdx", arg2StackLoc)
                } else sb ++= fmt("pushq", "%rdx")
                if (rcxUsed) {
                  sb ++= arg2StackInstrs
                  sb ++= fmt("movq", "%rcx", arg2StackLoc)
                } else sb ++= fmt("pushq", "%rcx")
                sb ++= fmt("cqo")
                if (rcxUsed || rdxUsed) {
                  sb ++= fmt("movq", arg2StackLoc, "%rcx")
                } else {
                  sb ++= emitSubInstr(
                    "movq",
                    arg2,
                    Some(RegMaker.tmpBuiltInReg(RcxLoc)),
                    bb,
                    line
                  )
                }
                sb ++= fmt("idivq", "%rcx")
                sb ++= emitSubInstr(
                  "movq",
                  RegMaker.tmpBuiltInReg(RdxLoc),
                  Some(RegMaker.tmpBuiltInReg(RaxLoc)),
                  bb,
                  line,
                  false,
                  true
                )
                if (rcxUsed) {
                  sb ++= arg2StackInstrs
                  sb ++= fmt("movq", arg2StackLoc, "%rcx")
                } else sb ++= fmt("popq", "%rcx")
                if (rdxUsed) {
                  sb ++= arg2StackInstrs
                  sb ++= fmt("movq", arg2StackLoc, "%rdx")
                } else sb ++= fmt("popq", "%rdx")
              }
              case AndInstr(dest, arg1, arg2) => {
                sb ++= emitSubInstr(
                  "andq",
                  arg2,
                  Some(RegMaker.tmpBuiltInReg(RaxLoc)),
                  bb,
                  line
                )
              }
              case OrInstr(dest, arg1, arg2) => {
                sb ++= emitSubInstr(
                  "orq",
                  arg2,
                  Some(RegMaker.tmpBuiltInReg(RaxLoc)),
                  bb,
                  line
                )
              }
              case LeftShiftInstr(dest, arg1, arg2) => {
                sb ++= emitSubInstr(
                  "salq",
                  arg2,
                  Some(RegMaker.tmpBuiltInReg(RaxLoc)),
                  bb,
                  line
                )
              }
              case RightShiftInstr(dest, arg1, arg2) => {
                sb ++= emitSubInstr(
                  "sarq",
                  arg2,
                  Some(RegMaker.tmpBuiltInReg(RaxLoc)),
                  bb,
                  line
                )
              }
              case _ =>
                throw new Exception(s"unexpected arith instr $arithInstr")
            }
            sb ++= emitSubInstr(
              "movq",
              RegMaker.tmpBuiltInReg(RaxLoc),
              Some(arithInstr.dest),
              bb,
              line,
              false,
              true
            )
          }
          case cmpInstr: CmpInstr => {
            sb ++= emitSubInstr(
              "cmp",
              cmpInstr.arg2,
              Some(RegMaker.tmpBuiltInReg(RaxLoc)),
              bb,
              line
            )
            sb ++= emitSubInstr(
              "movq",
              IntConstReg(0),
              Some(RegMaker.tmpBuiltInReg(RaxLoc)),
              bb,
              line,
              false,
              true
            )
            cmpInstr match {
              case GtInstr(dest, arg1, arg2) => {
                sb ++= fmt("jle", s".shortJump${bb.id}${line}")
              }
              case LtInstr(dest, arg1, arg2) => {
                sb ++= fmt("jge", s".shortJump${bb.id}${line}")
              }
              case GeInstr(dest, arg1, arg2) => {
                sb ++= fmt("jl", s".shortJump${bb.id}${line}")
              }
              case LeInstr(dest, arg1, arg2) => {
                sb ++= fmt("jg", s".shortJump${bb.id}${line}")
              }
              case EqInstr(dest, arg1, arg2) => {
                sb ++= fmt("jne", s".shortJump${bb.id}${line}")
              }
              case NeInstr(dest, arg1, arg2) => {
                sb ++= fmt("je", s".shortJump${bb.id}${line}")
              }
              case _ =>
                throw new Exception(s"unexpected cmp instr $cmpInstr")
            }
            sb ++= emitSubInstr(
              "movq",
              IntConstReg(1),
              Some(RegMaker.tmpBuiltInReg(RaxLoc)),
              bb,
              line,
              false,
              true
            )
            sb ++= emitSubInstr(
              "movq",
              RegMaker.tmpBuiltInReg(RaxLoc),
              Some(cmpInstr.dest),
              bb,
              line,
              false,
              true
            )
            sb ++= s".shortJump${bb.id}${line}:\n"
          }
        }
      }
      case NoOpInstr() => sb
      case NegInstr(dest, arg) => {
        sb ++= emitSubInstr(
          "movq",
          arg,
          Some(RegMaker.tmpBuiltInReg(RaxLoc)),
          bb,
          line
        )
        sb ++= fmt("neg", "%rax")
        sb ++= emitSubInstr(
          "movq",
          RegMaker.tmpBuiltInReg(RaxLoc),
          Some(dest),
          bb,
          line,
          false,
          true
        )
      }
      case NotInstr(dest, arg) => {
        sb ++= emitSubInstr(
          "movq",
          arg,
          Some(RegMaker.tmpBuiltInReg(RaxLoc)),
          bb,
          line
        )
        sb ++= fmt("xorq", "$1", "%rax")
        sb ++= emitSubInstr(
          "movq",
          RegMaker.tmpBuiltInReg(RaxLoc),
          Some(dest),
          bb,
          line,
          false,
          true
        )
      }
      case CopyInstr(dest, arg) => {
        if (arg != dest) {
          sb ++= emitSubInstr(
            "movq",
            arg,
            Some(RegMaker.tmpBuiltInReg(RaxLoc)),
            bb,
            line
          )
          sb ++= emitSubInstr(
            "movq",
            RegMaker.tmpBuiltInReg(RaxLoc),
            Some(dest),
            bb,
            line,
            false,
            true
          )
        }
      }
      case IncrInstr(dest, arg) => {
        sb ++= emitSubInstr("incq", dest, None, bb, line, true)
      }
      case DecrInstr(dest, arg) => {
        sb ++= emitSubInstr("decq", dest, None, bb, line, true)
      }
      case CallInstr(funcName, dest, args) => {
        val paramsToStore = args.length match {
          case a if a < callerSaved.length =>
            (callerSaved.length - bb.functionHeader.get.numExcessParams) +
              (bb.functionHeader.get.numExcessParams
                .min(bb.functionHeader.get.maxColor + 1))
          case _ => args.length
        }
        var skippedParams = 0
        // save caller-saved registers
        for (reg <- callerSaved.reverse.take(args.length)) {
          if (regLocation(dest, bb, line, true)._1 == reg.toString) {
            skippedParams += 1
          } else {
            sb ++= fmt("pushq", s"$reg")
          }
        }
        for (reg <- callerSaved.take(paramsToStore - args.length)) {
          if (regLocation(dest, bb, line, true)._1 == reg.toString) {
            skippedParams += 1
          } else {
            sb ++= fmt("pushq", s"$reg")
          }
        }
        // place first 6 args into the special registers
        for ((arg, i) <- args.take(paramsToStore).zipWithIndex) {
          val opStr = arg match {
            case StrFormat                    => "leaq"
            case StrAddr(string, location, _) => "leaq"
            case b: ArrBaseAddr => {
              b.location match {
                case AddrLocation.Data  => "leaq"
                case AddrLocation.Stack => "movq"
                case _ =>
                  throw new Exception(s"unexpected addr location ${b.location}")
              }
            }
            case _ => "movq"
          }
          if (i < callerSaved.length) {
            sb ++= emitSubInstr(
              opStr,
              arg,
              Some(RegMaker.tmpBuiltInReg(RegLocInfo.paramRegLocs(i))),
              bb,
              line
            )
          }
        }
        // keep the stack 16-byte aligned
        val modArgs = (paramsToStore - skippedParams) % 2
        if (modArgs == 1)
          sb ++= emitSubInstr(
            "pushq",
            IntConstReg(0),
            None,
            bb,
            line
          )
        // place the rest of the args into the stack in reverse order
        // i.e. the 7th arg is at the top of the stack
        for ((arg, i) <- args.zipWithIndex.reverse) {
          if (i >= callerSaved.length) {
            sb ++= emitSubInstr("pushq", arg, None, bb, line)
          }
        }
        // see Piazza @170, all imported functions should 0 rax
        sb ++= fmt("xorq", "%rax", "%rax")
        sb ++= fmt("call", callName(funcName))
        // pop off stack
        val numPopArgs =
          modArgs + (0).max(
            (paramsToStore - skippedParams) - callerSaved.length
          )
        if (numPopArgs > 0) {
          sb ++= emitSubInstr(
            "add",
            IntConstReg(RegAllocator.stackStride * numPopArgs),
            Some(RegMaker.tmpBuiltInReg(RspLoc)),
            bb,
            line
          )
        }
        // restore caller-saved registers
        for (reg <- callerSaved.take(paramsToStore - args.length).reverse) {
          if (regLocation(dest, bb, line, true)._1 != reg.toString) {
            sb ++= fmt("popq", s"$reg")
          }
        }
        for (reg <- callerSaved.reverse.take(args.length).reverse) {
          if (regLocation(dest, bb, line, true)._1 != reg.toString) {
            sb ++= fmt("popq", s"$reg")
          }
        }
        sb ++= emitSubInstr(
          "movq",
          RegMaker.tmpBuiltInReg(RaxLoc),
          Some(dest),
          bb,
          line,
          false,
          true
        )
      }
      case RetInstr(arg) => {
        sb ++= emitSubInstr(
          "movq",
          arg,
          Some(RegMaker.tmpBuiltInReg(RaxLoc)),
          bb,
          line
        )
        if (
          calleeSaved
            .take(
              1 + bb.functionHeader.get.maxColor - bb.functionHeader.get.numExcessParams
            )
            .length % 2 == 1
        )
          sb ++= emitSubInstr(
            "add",
            IntConstReg(RegAllocator.stackStride),
            Some(RegMaker.tmpBuiltInReg(RspLoc)),
            bb,
            line
          )
        for (
          reg <- calleeSaved
            .take(
              1 + bb.functionHeader.get.maxColor - bb.functionHeader.get.numExcessParams
            )
            .reverse
        ) {
          sb ++= fmt("popq", reg)
        }
        sb ++= fmt("subq", s"$$${CFGAllocator.MaxRspTable.get(name)}", "%rsp")
        sb ++= fmt("leave")
        sb ++= fmt("ret")
      }
      case LenInstr(dest, arg) => {
        sb ++= emitSubInstr(
          "movq",
          arg,
          Some(RegMaker.tmpBuiltInReg(RaxLoc)),
          bb,
          line
        )
        sb ++= emitSubInstr(
          "movq",
          RegMaker.tmpBuiltInReg(RaxLoc),
          Some(dest),
          bb,
          line,
          false,
          true
        )
      }
      case DeclInstr(reg, size) => {
        reg match {
          case _: ArrBaseAddr => {
            // Array base offsets are always static
            val offset =
              RegAllocator.getLoc(reg).asInstanceOf[StaticMemLoc[Long]]
            sb ++= emitSubInstr(
              "movq",
              IntConstReg(size),
              Some(reg),
              bb,
              line,
              false,
              true
            )
            for (i <- 1L to size) {
              sb ++= fmt(
                "movq",
                "$0",
                s"${offset.value + i * RegAllocator.stackStride}(%rbp)"
              )
            }
          }
          case _: ScalarAddr =>
            sb ++= emitSubInstr(
              "movq",
              IntConstReg(0),
              Some(reg),
              bb,
              line,
              false,
              true
            )
          case _ =>
        }
      }
      case ArrCheckInstr(arrElem: ArrElemAddr) => {
        val o = RegAllocator.getLoc(arrElem).asInstanceOf[ArrAddrOffset]
        val (ind, instrs) =
          regLocation(o.index.src, bb, line, false)
        sb ++= instrs
        sb ++= fmt("movq", ind, "%r11")
        arrElem.location match {
          case AddrLocation.Stack => {
            sb ++= fmt("cmp", s"${o.base.value}(%rbp)", "%r11")
          }
          case AddrLocation.Data => {
            sb ++= fmt("cmp", s"${Header.data}${o.base.value}", "%r11")
          }
        }
        sb ++= fmt("jge", Header.failedIndex)
        sb ++= fmt("cmp", "$0", "%r11")
        sb ++= fmt("jl", Header.failedIndex)
      }
      case EmptyInstr =>
      case _          => throw new Exception(s"Unsupported instruction: $instr")
    }
    sb.toString()
  }

  // Helper for emitInstr
  def emitSubInstr(
      op: String,
      arg1: Register,
      arg2: Option[Register],
      bb: BasicBlock,
      line: Int,
      arg1IsDef: Boolean = false,
      arg2IsDef: Boolean = false
  ): String = {
    val sb = new StringBuilder()
    val (arg1Loc, arg1Instrs) = regLocation(arg1, bb, line, arg1IsDef)
    sb ++= arg1Instrs
    if (arg2.nonEmpty) {
      val (arg2Loc, arg2Instrs) = regLocation(arg2.get, bb, line, arg2IsDef)
      sb ++= arg2Instrs
      sb ++= fmt(op, arg1Loc, arg2Loc)
    } else {
      sb ++= fmt(op, arg1Loc)
    }
    /* Remove redundant moves (e.g., the last line below)
    movq    %rax, %r14
    # 1: t28: stack[-64] = t27: stack[-56] + j: stack[-16]
    movq    %r14, %rax
     */
    val prevDestOpt: Option[Register] = prevInstr match {
      case Some(destInstr: DestInstr) => Some(destInstr.dest)
      case _                          => None
    }
    val arg2HasRax = arg2 == Some(RegMaker.tmpBuiltInReg(RaxLoc))
    if (op == "movq" && prevDestOpt == Some(arg1) && arg2HasRax) {
      // comment out redundant move
      val comment = "#\t"
      sb.toString().split("\n").map(comment + _).mkString("\n") + "\n"
    } else {
      sb.toString()
    }
  }
}

object CodeGenerator extends ProgramGenerator {
  var regAlloc = false
}
