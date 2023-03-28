import org.scalatest.FunSuite

import unittests._
import scala.math.abs

import edu.mit.compilers.codegen._
import edu.mit.compilers.parser._
import edu.mit.compilers.semantics._

class WebberTester extends FunSuite {
  test("Generic merge into groups works") {
    // Merge if they have any overlap
    def mergeable(i: Int, j: Int) = abs(i - j) <= 2
    val merger: Merger[Int] = new Merger[Int](mergeable)

    // Mergeable sets
    val a: Int = 1
    val b: Int = 2
    val c: Int = 3
    // `d` should be able to be merged with c and therefore with the rest
    val d: Int = 5

    assert(merger.canMerge(Set(a), Set(b)) == true)
    assert(merger.canMerge(Set(a), Set(c)) == true)
    assert(merger.canMerge(Set(a), Set(d)) == false)
    assert(merger.canMerge(Set(b), Set(c)) == true)
    assert(merger.canMerge(Set(b), Set(d)) == false)
    assert(merger.canMerge(Set(c), Set(d)) == true)

    // Not mergeable
    val x: Int = 10

    assert(merger.canMerge(Set(a), Set(x)) == false)
    assert(merger.canMerge(Set(b), Set(x)) == false)
    assert(merger.canMerge(Set(c), Set(x)) == false)
    assert(merger.canMerge(Set(d), Set(x)) == false)

    // Grouping should be into 2 groups
    val expectedGroups: Set[Set[Int]] =
      Set(Set(a, b, c, d), Set(x))
    val groups = merger.group(Set(a, b, c, d, x))
    assert(groups == expectedGroups)
  }

  test("Merging a pair of chains in the same block works") {
    val (bbs, regs) = SampleCFGs.smallSingletonFx()
    assert(bbs.size > 0)
    assert(regs.size > 0)
    val bb = bbs(0)
    val reg1 = regs(0)

    val chains: Set[Chain] = Chainer.mk(bb)

    // reg1: 1 -> 2
    // reg2: nothing
    assert(chains.size == 1)

    // reg1: 1 -> 2
    val groups: List[Set[Chain]] =
      Webber.mergeChains(chains).map(_.chains).toList
    assert(groups.size == 1)
    assert(groups.head.size == 1)
    assert(groups.head.head.definition.reg == reg1)
    assert(groups.head.head.usage.reg == reg1)

    // Try big singleton with these def-use chains:
    // 1. reg1: {(1, 2), (1, 3), (1, 6)}
    // 2. reg2: {(3, 5)}
    // 3. reg3: {(2, 4)}

    val (bbs2, regs2) = SampleCFGs.bigSingletonFx()
    assert(bbs2.size == 1)
    assert(regs2.size == 3)
    val bb2 = bbs2(0)
    val (reg1_2, reg2_2, reg3_2) = (regs2(0), regs2(1), regs2(2))

    val chains2: Set[Chain] = Chainer.mk(bb2)
    val groups2: List[Set[Chain]] =
      Webber.mergeChains(chains2).map(_.chains).toList
    // Make sure we have the right number of groups
    assert(groups2.size == 3)
    assert(groups2.map(_.size).sum == 5)
    assert(groups2.map(_.size).max == 3)
    assert(groups2.map(_.size).min == 1)

    // Make sure that register 1 has a size 3 group and registers 2 and 3 have size 1 groups
    assert(groups2.find(_.head.definition.reg == reg1_2).get.size == 3)
    assert(groups2.find(_.head.definition.reg == reg2_2).get.size == 1)
    assert(groups2.find(_.head.definition.reg == reg3_2).get.size == 1)
  }

  test(
    "Merging chains in different blocks (DAG) works (with singleton contours)"
  ) {
    val (bbs, regs) = SampleCFGs.forkFx()
    assert(bbs.size == 4)
    assert(regs.size == 2)
    val cfgHeader = bbs(0)
    val (reg1, reg2) = (regs(0), regs(1))

    // Expect defs and uses (chains)
    // (where the blocks are F: fork, B: body, E: end)
    // reg1: (0F, 1F), (0F, 0B), (0E, 2E)
    // reg2: (0B, 0E)

    val chains: Set[Chain] = Chainer.mk(cfgHeader)
    assert(chains.filter(_.definition.reg == reg1).size == 3)
    assert(chains.filter(_.definition.reg == reg2).size == 1)

    // Expect def/uses to merge into
    // reg1: {{(0F, 1F), (0F, 0B)}, {(0E, 2E)}}
    // reg2: {{(0B, 0E)}}
    val groups: List[Set[Chain]] =
      Webber.mergeChains(chains).map(_.chains).toList
    // Two contours and each has one def and one use
    assert(groups.map(_.size).max == 2)
    assert(groups.map(_.size).min == 1)
    // Two groups for reg1 and one for reg2
    assert(groups.filter(_.head.usage.reg == reg1).size == 2)
    assert(groups.filter(_.head.usage.reg == reg2).size == 1)
  }

  test(
    "Merging a pair of chains in different blocks with a loop works (and multiple def/uses per contour)"
  ) {
    val (bbs, _) = SampleCFGs.loopFx()
    assert(bbs.size == 4)
    val cfgHeader = bbs(0)
    val chains: Set[Chain] = Chainer.mk(cfgHeader)
    // NOTE that there should be a backloop

    // Expect Minimal def uses to be:
    // reg1: {(0PH, 0H), (0PH, 0L), (0PH, 0E), (0L, 1L), (0L, 0H), (0L, 0L), (0L, 0E)}
    // reg2: {(1L, 0L), (1L, 0E)}

    // Expect them to merge into:
    // reg1: {{(0PH, 0H), (0PH, 0L), (0PH, 0E), (0L, 1L), (0L, 0E), (0L, 0L), (0L, 0E)}} <- note they all merge because of the class algorithm
    // reg2: {{(1L, 0L), (1L, 0E)}}
    val groups: List[Set[Chain]] =
      Webber.mergeChains(chains).map(_.chains).toList
    assert(groups.size == 2)

    assert(groups.map(_.size).max == 7)
    assert(groups.map(_.size).min == 2)
  }
}
