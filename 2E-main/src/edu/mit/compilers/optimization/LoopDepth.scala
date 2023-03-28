package edu.mit.compilers.optimization

import edu.mit.compilers.codegen._
import scala.math.pow

object LoopDepth {
  // RegAllocation opt: helper to recursively determine loop depth
  def getLoopDepth(bb: BasicBlock): Int = {
    if (bb.loop.isEmpty) {
      0
    } else {
      1 + getLoopDepth(bb.loop.get)
    }
  }

  // RegAllocation opt: Gets spill cost for variables in block
  def getSpillCost(bb: BasicBlock): Int = {
    // 10, 100, 1000, etc...
    val loopDepth: Int = getLoopDepth(bb)
    if (loopDepth > 9) {
      Integer.MAX_VALUE
    } else {
      pow(10, loopDepth).intValue
    }
  }
}
