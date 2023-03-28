package edu.mit.compilers.optimization

import edu.mit.compilers.codegen._

// Copy Propagation
case object RegAlloc extends Optimization {

  override def opt(cfg: BasicBlock): BasicBlock = {
    CodeGenerator.regAlloc = true
    cfg
  }

  override def toString(): String = "regalloc"

  override def precedence(): Int = Precedences.REG_ALLOC
}
