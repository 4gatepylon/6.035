package edu.mit.compilers.optimization

import edu.mit.compilers.codegen._

case object UsedRegOps {
  // This is an optimistic algorithm that assumes (at first) that nothing is used and
  // then adds things as it moves backwards and sees that things are used.
  def bottom: Set[Register] = {
    Set()
  }

  // Least upper bound is a union (i.e. you should keep everything that is
  // used by ANY successor)
  def lubInstr(used1: Set[Register], used2: Set[Register]): Set[Register] = {
    used1 ++ used2
  }

  // For a collection
  def lubBlock(ls: List[Set[Register]]): Set[Register] = {
    ls.foldLeft(bottom)((lacc, l) => lubInstr(lacc, l))
  }

  def lubIncoming(
      bb: BasicBlock,
      outgoings: Map[BasicBlock, List[Set[Register]]]
  ): Set[Register] = {
    lubBlock(
      bb.children
        .map(succ => {
          val incoming_from = outgoings(succ)
          if (incoming_from.size == 0) {
            throw new Exception(
              s"incoming had size 0 inside lub incoming child ${succ.id}"
            )
          } else {
            incoming_from.head
          }
        })
        .toList
    )
  }

  def keepInstr(instr: BasicInstr, uses: Set[Register]): Boolean = {
    instr match {
      // Calls may modify globals or be callouts, so they are always kept
      case instr: CallInstr     => true
      case instr: DeclInstr     => uses.contains(instr.reg)
      case instr: ArrCheckInstr => uses.contains(instr.arrElem)
      // Note array destinations are always "used"
      case instr: DestInstr => {
        // println(
        //   s"**************** checking if dest ${instr.dest} is used now or later"
        // )
        val u: Boolean = uses.contains(instr.dest)
        // println(s"**************** got that it was used: $u")
        u
      }
      case _ => true
    }
  }

  def removeUnusedInstrs(bb: BasicBlock, uses: List[Set[Register]]): Unit = {
    // The prefix is that inclusive of each instruction
    val usesIncl: List[Set[Register]] = uses.dropRight(1)
    val instrs: List[BasicInstr] =
      (bb.instrs, usesIncl).zipped
        .map((instr: BasicInstr, use: Set[Register]) => {
          if (keepInstr(instr, use)) {
            instr
          } else {
            NoOpInstr()
          }
        })
        .filterNot(_.isInstanceOf[NoOpInstr])
    bb.instrs = instrs
  }

  // Given a register, find all the ones that this depends on
  // (so that we can add them to `used`)
  def dependencies(arg: Register): List[Register] = {
    arg match {
      // the below change solved x-29 and x-18
      case arg: ArrElemAddr => List(arg, arg.index, arg.base)
      case arg: ConstReg    => List()
      case arg              => List(arg)
    }
  }

  // Given a set of things used after an instruction, find what things are used at
  // or after that instruction (so exclusive of that instruction to inclusive).
  def tfInstr(used: Set[Register], instr: BasicInstr): Set[Register] = {
    instr match {
      case RetInstr(arg) => {
        used ++ dependencies(arg)
      }
      case instr: CallInstr => {
        // Without inlining, we can't really know whether the
        // arguments are used or not. Even if the function's output
        // is not used, it might modify globals.
        used ++ instr.args.flatMap(dependencies) ++ dependencies(instr.dest)
        // the above fixed 08-arrays
      }
      case instr: DestInstr => {
        // Add the destinations if necessary
        val addDest: List[Register] = instr.dest match {
          // We must add the destination and its dependency even if it's not already present
          case dest: ArrElemAddr => {
            val deps = dependencies(dest)
            // println(
            //   s"********************* adding as dest ${deps} *****************"
            // )
            deps
          }
          case dest: Addr => {
            dest.location match {
              case AddrLocation.Data => dependencies(dest)
              case _                 => List()
            }
          }
          case _ => {
            List()
          }
        }
        // Add the args (based on whether the destination was added at some point in the past)
        val addArgs: List[Register] =
          // The below change fixed 17-insertionsort && x-23 nested
          if (!addDest.isEmpty || used.contains(instr.dest))
            instr match {
              case instr: BinOpInstr => {
                val deps =
                  dependencies(instr.arg1) ++ dependencies(instr.arg2)
                // println(
                //   s"********************* adding ${deps} *****************"
                // )
                deps
              }
              case instr: UnOpInstr => {
                val deps = dependencies(instr.arg)
                // println(
                //   s"********************* adding ${deps} *****************"
                // )
                deps
              }
              case instr: CopyInstr => {
                val deps = dependencies(instr.arg)
                // println(
                //   s"********************* adding ${deps} *****************"
                // )
                deps
              }
              case _ => {
                List()
              }
            }
          else List()
        val add: Set[Register] = (addDest ++ addArgs).toSet
        used ++ add
      }
      case instr: ArrCheckInstr => {
        // This fixed x-25
        used ++ dependencies(instr.arrElem)
      }
      case _ => {
        // Arr check instructions fit in this case because
        // they will be added if they are a destination.
        used
      }
    }
  }

  // Tranfer function of asequence of a basic block just goes over the sequence of the basic instructions
  def tfBlock(incoming: Set[Register], bb: BasicBlock): List[Set[Register]] = {
    // Even if the register whose value we jump on is unused, we still need it to jump
    val withJump: Set[Register] = bb match {
      // Turns out flag for fork could have been helpful :P
      case fb: ForkBlock => {
        val dest = fb.condDest.getOrElse(
          throw new Exception(
            s"Fork ${fb.id} has no destination for the conditional value"
          )
        )
        incoming ++ dependencies(dest).toSet
      }
      case _ => {
        incoming
      }
    }
    bb.instrs.size match {
      case 0 => {
        // The empty block changes nothing
        List(withJump)
      }
      case _ => {
        val accs = bb.instrs.reverse.foldLeft(List(withJump)) { (acc, instr) =>
          acc :+ tfInstr(acc.last, instr)
        }
        // Return in same order as instructions (they interleave them)
        accs.reverse
      }
    }
  }

  // Return the outgoing values of every cfg node
  def fixedPoint(cfg: BasicBlock): Map[BasicBlock, List[Set[Register]]] = {
    assert(
      cfg.functionHeader.getOrElse(
        throw new Exception("Provide function header to `DCE.fixedPoint`")
      ) == cfg,
      "Need to be given function header (CFG) to `DCE.fixedPoint`"
    )

    var worklist: List[BasicBlock] = cfg.visit(basicBlock => basicBlock)

    // An outgoing map that tells you the outgoing lattice value previously
    var outgoings: Map[BasicBlock, List[Set[Register]]] =
      worklist.map(bb => (bb, List.fill(bb.instrs.size + 1)(bottom))).toMap

    // A "map" function that tells you the incoming lattice value
    def incoming(bb: BasicBlock): Set[Register] = {
      // Remember that the outgoings are in the same order as as the instructions (interleaved)
      val children = bb.children
      // println(
      //   s"********************** children of ${bb.id}: ${children.map(_.id)}***************"
      // )
      lubIncoming(bb, outgoings)
    }

    // While things change, make sure to continue
    // (if across a basic block nothing changes, then
    // within it it can't either, so we only need to
    // check blocks like in lecture)
    while (worklist.size > 0) {
      // println(
      //   s"*********************** worklist starts as ${worklist.map(_.id)} ***********************"
      // )
      val n = worklist.head
      worklist = worklist.drop(1)
      // println(
      //   s"************************* head ${n.id} outgoings \n${outgoings(n).mkString(",")}\n *************************"
      // )

      val prev_outgoings: List[Set[Register]] = outgoings(n)
      // println(s"**** ${n.id}")
      val new_incoming: Set[Register] = incoming(n)
      // println(
      //   s"************************* node ${n.id} new_incoming: ${new_incoming} *************************"
      // )
      val new_outgoings: List[Set[Register]] = tfBlock(new_incoming, n)
      if (prev_outgoings != new_outgoings) {
        outgoings = outgoings - n
        outgoings = outgoings + (n -> new_outgoings)
        // println(
        //   s"********************* adding for node ${n.id} outgoing ${new_outgoings.head} *********************"
        // )
        // NOTE: this moves backwards
        worklist = worklist ++ n.parents
      }
    }
    outgoings
  }
}

// Dead Code Elimination
case object DCE extends Optimization {
  override def opt(cfg: BasicBlock): BasicBlock = {
    assert(
      cfg.functionHeader.getOrElse(
        throw new Exception("Provide function header to `DCE.opt`")
      ) == cfg,
      "Need to be given function header (CFG) to `DCE.opt`"
    )

    // Find what is used at every block
    val usedPerBlock: Map[BasicBlock, List[Set[Register]]] =
      UsedRegOps.fixedPoint(cfg)
    // TODO remove this debugging
    // println("************************** USED PER BLOCK")
    // println(
    //   usedPerBlock.toList
    //     .map(_ match {
    //       case (bb, used) =>
    //         s"${bb.id} -> \n\t${(bb.instrs, used).zipped.toList.mkString("\n\t")}"
    //     })
    //     .mkString("\n")
    // )
    // println("*************************** LUB OF INCOMING FROM CHILDREN")
    // for ((bb, used) <- usedPerBlock) {
    //   val lub: Set[Register] = UsedRegOps.lubIncoming(bb, usedPerBlock)
    //   println(s"${bb.id} <- ${lub}")
    //   for (child <- bb.children) {
    //     val used = usedPerBlock(child)
    //     println(s"child ${child.id} -> ${used.head}")
    //   }
    // }
    // println("******************************")
    // Remove everything that is unused
    cfg.visit[Unit]((basicBlock) =>
      UsedRegOps.removeUnusedInstrs(basicBlock, usedPerBlock(basicBlock))
    )
    cfg
  }
  override def toString(): String = "dce"

  override def precedence(): Int = Precedences.DCE
}
