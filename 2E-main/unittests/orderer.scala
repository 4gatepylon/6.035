import edu.mit.compilers.codegen._
import org.scalatest.FunSuite

import unittests._

// Look at this (the point is to get every pair of bbs)
// https://stackoverflow.com/questions/14740199/cross-product-in-scala
case object Cross {
  def apply[T1, T2](x: Iterable[T1], y: Iterable[T2]): Iterable[(T1, T2)] = {
    // Not safe for empty lists
    x.flatMap(x => y.map(y => (x, y)))
  }
}

case object StrictOrderingInBlock {
// Check that within a block all instructions are strictly ordered
// in a monotonic ordering.
  def apply(
      bb: BasicBlock,
      ordering: Map[BasicBlock, Map[BasicInstr, InstrOrder]]
  ): Boolean = {
    if (bb.instrs.size < 2) {
      true
    } else {
      bb.instrs
        .sliding(2)
        .foldLeft(true)((acc, pair) =>
          acc &&
            ordering(bb)(pair(0)) <
            ordering(bb)(pair(1))
        )
    }
  }
}

case object StrictOrderingAcrossBlocksInstructions {
  // Check that for a pair of blocks that are not the same
  // whichever block beats the other will have the property that
  // all its instructions beat all the other block's instructions.
  // If neither beats eachother, then we return true.
  def apply(
      bb1: BasicBlock,
      bb2: BasicBlock,
      ordering: Map[BasicBlock, Map[BasicInstr, InstrOrder]]
  ): Boolean = {
    assert(bb1 != bb2)
    // Check who is winning
    val block_ordering = BlockOrderer.order(
      bb1.functionHeader.getOrElse(
        throw new Exception(
          "no function header in `StrictOrderingAcrossBlocksInstructions in orderer unit testing utility`"
        )
      )
    )
    val bb1_beats = block_ordering(bb1) < block_ordering(bb2)
    val bb2_beats = block_ordering(bb2) < block_ordering(bb1)

    // Get all pairs of instructions
    val bb1_instrs = bb1.instrs.toList
    val bb2_instrs = bb2.instrs.toList
    if (bb1_instrs.size == 0 || bb2_instrs.size == 0) {
      true
    } else {
      val instr_pairs = Cross.apply(bb1_instrs, bb2_instrs)
      instr_pairs.foldLeft(true)((acc, pair) => {
        val (bb1_instr, bb2_instr) = pair
        val bb1_instr_order = ordering(bb1)(bb1_instr)
        val bb2_instr_order = ordering(bb2)(bb2_instr)
        acc && (
          // Either bb1 beats in which case the bb1_instr must win
          ((bb1_instr_order < bb2_instr_order) && bb1_beats) ||
            // Or bb2 beats in which case the bb2_instr must win
            ((bb1_instr_order > bb2_instr_order) && bb2_beats) ||
            // Or no one beats in which case any order is OK
            (!bb1_beats && !bb2_beats)
        )
      })
    }
  }
}

class BlockOrdererTestSuite extends FunSuite {
  test("Test block orderer on single block") {
    val (small_bbs, _) = SampleCFGs.smallSingletonFx()
    val (big_bbs, _) = SampleCFGs.bigSingletonFx()
    assert(small_bbs.size == 1)
    assert(big_bbs.size == 1)
    val big_bb = big_bbs.head
    val small_bb = small_bbs.head

    val big_ordering: Map[BasicBlock, Long] = BlockOrderer.order(big_bb)
    assert(big_ordering.size == 1)

    val small_ordering: Map[BasicBlock, Long] = BlockOrderer.order(small_bb)
    assert(small_ordering.size == 1)
  }
  test("Test block orderer on multiple blocks (dag)") {
    val (bbs, _) = SampleCFGs.forkFx()
    assert(bbs.size == 4)
    val (fxHeader, fork, body, end) = (bbs.head, bbs(1), bbs(2), bbs(3))
    val ordering = BlockOrderer.order(fxHeader)
    assert(ordering.size == 4)
    assert(ordering(fxHeader) < ordering(fork))
    assert(ordering(fork) < ordering(body))
    assert(ordering(body) < ordering(end))
  }
  test("Test block orderer on multiple blocks (loop)") {
    val (bbs, _) = SampleCFGs.loopFx()
    assert(bbs.size == 4)
    val (fxHeader, loop, body, end) = (bbs.head, bbs(1), bbs(2), bbs(3))
    val ordering = BlockOrderer.order(fxHeader)
    assert(ordering.size == 4)
    assert(ordering(fxHeader) < ordering(loop))
    assert(ordering(loop) < ordering(body))
    assert(ordering(loop) < ordering(end))
  }
  test(
    "Test block orderer on multiple blocks with a nested if statement (short path to outer end)"
  ) {
    val (bbs, _) = SampleCFGs.nestedForkFx()
    assert(bbs.size == 6)
    val (fxHeader, outerFork, innerFork, body, innerEnd, outerEnd) =
      (bbs.head, bbs(1), bbs(2), bbs(3), bbs(4), bbs(5))

    val ordering = BlockOrderer.order(fxHeader)
    assert(ordering.size == 6)
    assert(ordering(fxHeader) < ordering(outerFork))
    assert(ordering(fxHeader) < ordering(outerEnd))
    assert(ordering(outerFork) < ordering(innerFork))
    assert(ordering(innerFork) < ordering(body))
    assert(ordering(innerFork) < ordering(innerEnd))
    assert(ordering(body) < ordering(innerEnd))
    assert(ordering(innerEnd) < ordering(outerEnd))
  }
}

class InstructionOrdererTestSuite extends FunSuite {
  test("Test instr orderer on single block") {
    val (small_bbs, _) = SampleCFGs.smallSingletonFx()
    val (big_bbs, _) = SampleCFGs.bigSingletonFx()
    assert(small_bbs.size == 1)
    assert(big_bbs.size == 1)
    val big_bb = big_bbs.head
    val small_bb = small_bbs.head

    val big_ordering: Map[BasicBlock, Map[BasicInstr, InstrOrder]] =
      InstrOrderer.order(big_bb)
    assert(big_ordering.size == 1)
    assert(big_ordering(big_bb).size >= 1)
    assert(StrictOrderingInBlock.apply(big_bb, big_ordering))

    val small_ordering: Map[BasicBlock, Map[BasicInstr, InstrOrder]] =
      InstrOrderer.order(small_bb)
    assert(small_ordering.size == 1)
    assert(small_ordering(small_bb).size >= 1)
    assert(StrictOrderingInBlock.apply(small_bb, small_ordering))
  }
  test("Test instr orderer on multiple blocks (dag and loop)") {
    val (fbbs, _) = SampleCFGs.forkFx()
    val (ffbbs, _) = SampleCFGs.nestedForkFx()
    val (lbbs, _) = SampleCFGs.loopFx()
    assert(fbbs.size == 4 && lbbs.size == 4 && ffbbs.size == 6)

    // Get the orderings
    val fordering = InstrOrderer.order(fbbs(0))
    val lordering = InstrOrderer.order(lbbs(0))
    val ffordering = InstrOrderer.order(ffbbs(0))
    assert(fordering.size == 4 && lordering.size == 4 && ffordering.size == 6)

    // Organize into one loop
    val orderings = List(fordering, lordering, ffordering)

    // For all orderings (loop and dag)
    assert(orderings.foldLeft(true)((order_acc, ordering) => {
      val bbs = ordering.keySet
      val bbs_cross = Cross.apply(bbs, bbs)

      // For all pairs of blocks in that ordering
      order_acc && bbs_cross.foldLeft(true)((block_acc, pair) => {
        block_acc && (pair._1 == pair._2) match {
          case true => {
            // If they are in the same block, just make sure that they follow a strict ordering
            StrictOrderingInBlock.apply(pair._1, ordering)
          }
          case false => {
            // For all instructions in those pairs of blocks
            // make sure that if bb1 beats, then all bb1 instructions beat
            // all in bb2 and vice versa.
            StrictOrderingAcrossBlocksInstructions.apply(
              pair._1,
              pair._2,
              ordering
            )
          }
        }
      })
    }))
  }
}
