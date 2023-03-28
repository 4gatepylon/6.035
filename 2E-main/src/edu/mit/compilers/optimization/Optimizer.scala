package edu.mit.compilers.optimization

import edu.mit.compilers.codegen._
import scala.collection.mutable.ListBuffer

trait Optimization {
  // Take in the first basic block of a basic instruction and return the first
  // basic block of an optimized (transformed) version of the code. The code should
  // maintain the same semantics as the original.
  def intersect[K, V](
      maps: List[Map[K, V]],
      start: Map[K, V] = Map[K, V]()
  ): Map[K, V] = maps.fold(start) {
    case (acc, cur) =>
      (acc.keySet & cur.keySet)
        .filter(k => acc(k) == cur(k))
        .map(k => (k, acc(k)))
        .toMap
    case _ => throw new Exception("key value pair not found")
  }

  def opt(cfg: BasicBlock): BasicBlock
  def toString(): String

  // Higher precedence means it will be done earlier (we will sort in
  // descending order)
  def precedence(): Int
}

case object Precedences {
  val REG_ALLOC: Int = 5
  val CSE: Int = 4
  val CP: Int = 3
  val DCE: Int = 2
  val ALG_SIMP: Int = 1
}

// NOTE We will be moving this to its own namespace later
case object CFGOptimizer {
  var optimizations: List[Optimization] = List()
  def add(opt: Optimization): Unit = {
    optimizations = optimizations :+ opt
  }
  def opt(cfg: BasicBlock): BasicBlock = {
    var optCfg = cfg
    val numLoops = 5
    for (iter <- 0 until numLoops) {
      for (opt <- optimizations.sortBy(_.precedence()).reverse) {
        optCfg = opt.opt(optCfg)
      }
    }
    optCfg
  }
}
