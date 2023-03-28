package edu.mit.compilers.codegen

import edu.mit.compilers.parser._

/*
Source
c = a + b;

Flatten output
t1 = a + b;
c = t1;

Swap output
c = a + b;
t1 = c;
 */

object RegSwap {
  def cfg(bb: BasicBlock): Unit = bb.lineage.foreach(block)

  def block(bb: BasicBlock): Unit = {
    bb.instrs = instrs(bb.instrs)
  }

  def instrs(instrs: List[BasicInstr]): List[BasicInstr] = {
    instrs.sliding(2).foreach {
      case List(instr1: DestInstr, instr2: CopyInstr) => {
        (instr1.dest, instr2.arg) match {
          case (dest1: ScalarAddr, arg2: ScalarAddr)
              if dest1 == arg2 && dest1.varNameOpt == None => {
            instr1.dest = instr2.dest
            instr2.arg = instr2.dest
            instr2.dest = dest1
          }
          case _ =>
        }
      }
      case _ =>
    }
    instrs
  }
}
