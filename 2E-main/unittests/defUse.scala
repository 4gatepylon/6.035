import org.scalatest.FunSuite

import edu.mit.compilers.parser._
// Semantics necessary since we do end to end testing for a single decaf program
// to deal with the fact that some functionality is not available.
import edu.mit.compilers.semantics.{
  TableInserter,
  Checker,
  Symbols,
  Pemdas,
  Collector
}
import edu.mit.compilers.codegen._

import unittests._

class DefUseTestSuite extends FunSuite {
  test("Def Use Works in a single seq basic block (is function header)") {
    val (bbs, regs) = SampleCFGs.bigSingletonFx()
    assert(bbs.size == 1)
    assert(regs.size == 3)
    val bb = bbs(0)
    val (reg1, reg2, reg3) = (regs(0), regs(1), regs(2))

    val expectedDefsList: List[Def] = List(
      Def(reg1, 0, bb),
      Def(reg1, 1, bb),
      Def(reg3, 2, bb),
      Def(reg2, 3, bb),
      Def(reg3, 5, bb)
    )
    val expectedDefs: Set[Def] = expectedDefsList.toSet

    val expectedUsesList: List[Use] = List(
      // First instruction
      Use(reg2, 0, bb),
      Use(reg3, 0, bb),
      // Second insruction (nothing since they are constants)
      // Third instruction
      Use(reg1, 2, bb),
      // Fourth instruction
      Use(reg1, 3, bb),
      // Fifth instruction
      Use(reg3, 5, bb),
      Use(reg2, 5, bb),
      Use(reg1, 5, bb)
    )

    val expectedUses: Set[Use] = expectedUsesList.toSet

    // The chainer should use the functionality tested above

    val chainer: Chainer = Chainer(bb)
    assert(chainer.defs.size == 1)
    assert(chainer.uses.size == 1)
    assert(chainer.defs.contains(bb))
    assert(chainer.uses.contains(bb))
    assert(chainer.defs(bb).size == expectedDefs.size)
    assert(chainer.uses(bb).size == expectedUses.size)
    assert(chainer.defs(bb) == expectedDefs)
    assert(chainer.uses(bb) == expectedUses)

    // Expect the following chains to be found:
    // 1. reg1: {(1, 2), (1, 3), (1, 6)} -> the first definition is overwritten
    // 2. reg2: {(3, 5)}
    // 3. reg3: {(2, 4)}
    val chains: Set[Chain] = chainer.getChains()

    val expectedWebs: Set[Chain] = Set(
      // Reg 1
      Chain(expectedDefsList(1), expectedUsesList(2)),
      Chain(expectedDefsList(1), expectedUsesList(3)),
      Chain(expectedDefsList(1), expectedUsesList(6)),
      // Reg 2
      Chain(expectedDefsList(3), expectedUsesList(5)),
      // Reg 3
      Chain(expectedDefsList(2), expectedUsesList(4))
    )

    assert(chains == expectedWebs)
  }

  test(
    "Def Use Works with Multiple Basic Blocks: Diamond (works on split, works on join)"
  ) {
    val (bbs, regs) = SampleCFGs.forkFx()
    assert(bbs.size == 4)
    assert(regs.size == 2)
    val (cfgHeader, fork, body, end) = (bbs(0), bbs(1), bbs(2), bbs(3))
    val (reg1, reg2) = (regs(0), regs(1))

    val expectedDefsList: List[Def] = List(
      // Fork
      Def(reg1, 0, fork),
      // Body
      Def(reg2, 0, body),
      // End
      Def(reg1, 0, end),
      Def(reg2, 2, end)
    )

    val expectedUsesList: List[Use] = List(
      // Fork
      Use(reg1, 0, fork),
      Use(reg2, 0, fork),
      Use(reg1, 1, fork),
      // Body
      Use(reg1, 0, body),
      // End
      Use(reg2, 0, end),
      Use(reg1, 2, end)
    )

    val expectedDefs: Set[Def] = expectedDefsList.toSet
    val expectedUses: Set[Use] = expectedUsesList.toSet
    val chainer: Chainer = Chainer(cfgHeader)
    assert(chainer.defs.values.map(_.size).sum == expectedDefs.size)
    assert(chainer.uses.values.map(_.size).sum == expectedUses.size)
    assert(chainer.defs.contains(cfgHeader))
    assert(chainer.uses.contains(cfgHeader))
    assert(chainer.defs.contains(fork))
    assert(chainer.uses.contains(fork))
    assert(chainer.defs.contains(body))
    assert(chainer.uses.contains(body))
    assert(chainer.defs.contains(end))
    assert(chainer.uses.contains(end))
    assert(chainer.defs(cfgHeader).size == 0)
    assert(chainer.uses(cfgHeader).size == 0)
    assert(chainer.defs(fork).size == 1)
    assert(chainer.uses(fork).size == 3)
    assert(chainer.defs(body).size == 1)
    assert(chainer.uses(body).size == 1)
    assert(chainer.defs(end).size == 2)
    assert(chainer.uses(end).size == 2)
    val allDefs: Set[Def] =
      chainer.defs(fork) ++ chainer.defs(body) ++ chainer.defs(end)
    val allUses: Set[Use] =
      chainer.uses(fork) ++ chainer.uses(body) ++ chainer.uses(end)
    assert(allDefs == expectedDefs)
    assert(allUses == expectedUses)

    val chains: Set[Chain] = chainer.getChains()

    // Expect at this point
    // (where the blocks are F: fork, B: body, E: end)
    // reg1: (0F, 1F), (0F, 0B), (0E, 2E) <- the end block overwrites 0F
    // reg2: (0B, 0E)
    val expectedWebs = Set(
      // Reg 1
      Chain(expectedDefsList(0), expectedUsesList(2)), // (0F, 1F)
      Chain(expectedDefsList(0), expectedUsesList(3)), // (0F, 0B)
      Chain(expectedDefsList(2), expectedUsesList(5)), // (0E, 2E)
      // Reg 2
      Chain(expectedDefsList(1), expectedUsesList(4)) // (0B, 0E)
    )

    assert(chains == expectedWebs)
  }

  test("Def Use Finds Parameters") {
    val (bbs, regs) = SampleCFGs.paramFx()
    assert(bbs.size >= 1)
    assert(regs.size >= 1)
    val cfg = bbs(0)
    val a = regs(0)

    val chainer: Chainer = Chainer(cfg)
    assert(chainer.defs.size == 1)
    assert(chainer.uses.size == 1)
    assert(chainer.defs.contains(cfg))
    assert(chainer.uses.contains(cfg))
    val defsList = chainer.defs(cfg).toList
    val usesList = chainer.uses(cfg).toList

    assert(defsList.size >= 1)
    assert(usesList.size >= 1)
    // Make sure the parameter was detected
    assert(defsList.map(_.reg).contains(a))
    // Make sure the parameter was used
    assert(usesList.map(_.reg).contains(a))
  }

  test("Def Use Finds Loops backwards and conditional dest def-uses") {
    val (bbs, regs) = SampleCFGs.loopFx()
    assert(bbs.size == 4)
    assert(regs.size == 2)
    // cfgHeader is also a pre-header
    val (cfgHeader, header, loop, end) = (bbs(0), bbs(1), bbs(2), bbs(3))
    val (reg1, reg2) = (regs(0), regs(1))

    val expectedDefsList: List[Def] = List(
      // Pre-header
      Def(reg1, 0, cfgHeader),

      // No definitions in header

      // Loop
      Def(reg1, 0, loop),
      Def(reg2, 1, loop),

      // end
      Def(reg1, 0, end)
    )

    val expectedUsesList: List[Use] = List(
      // Pre-header
      Use(reg2, 0, cfgHeader),

      // Header
      Use(reg1, 0, header),

      // Loop
      Use(reg1, 0, loop),
      Use(reg2, 0, loop),
      Use(reg1, 1, loop),

      // End
      Use(reg1, 0, end),
      Use(reg2, 0, end)
    )

    val expectedDefs: Set[Def] = expectedDefsList.toSet
    val expectedUses: Set[Use] = expectedUsesList.toSet
    val chainer: Chainer = Chainer(cfgHeader)
    // All blocks should be present
    assert(chainer.defs.values.map(_.size).sum == expectedDefs.size)
    assert(chainer.uses.values.map(_.size).sum == expectedUses.size)
    assert(chainer.defs.contains(cfgHeader))
    assert(chainer.uses.contains(cfgHeader))
    assert(chainer.defs.contains(loop))
    assert(chainer.uses.contains(loop))
    assert(chainer.defs.contains(header))
    assert(chainer.uses.contains(header))
    assert(chainer.defs.contains(end))
    assert(chainer.uses.contains(end))

    // All the proper defs/uses should be detected including
    // the loop header
    val allDefs = chainer.defs.values.foldLeft(Set[Def]())(_ ++ _)
    val allUses = chainer.uses.values.foldLeft(Set[Use]())(_ ++ _)
    assert(allDefs == expectedDefs)
    assert(allUses == expectedUses)

    // Backwards dependency should include for web as well as
    // dependency on code after the loop (in end) on things both
    // in the header/pre-header and in the loop
    // Expect:
    // reg1: {(0PH, 0H), (0PH, 0L), (0PH, 0E), (0L, 1L), (0L, 0H), (0L, 0L), (0L, 0E)}
    // reg2: {(1L, 0L), (1L, 0E)}
    val chains: Set[Chain] = chainer.getChains()
    val expectedWebs: Set[Chain] = Set(
      // Reg1
      Chain(expectedDefsList(0), expectedUsesList(1)), // (0PH, 0H)
      Chain(expectedDefsList(0), expectedUsesList(2)), // (0PH, 0L)
      Chain(expectedDefsList(0), expectedUsesList(5)), // (0PH, 0E)
      Chain(expectedDefsList(1), expectedUsesList(4)), // (0L, 1L)
      Chain(expectedDefsList(1), expectedUsesList(1)), // (0L, 0H)
      Chain(expectedDefsList(1), expectedUsesList(2)), // (0L, 0L)
      Chain(expectedDefsList(1), expectedUsesList(5)), // (0L, 0E)
      // Reg2
      Chain(expectedDefsList(2), expectedUsesList(3)), // (1L, 0L)
      Chain(expectedDefsList(2), expectedUsesList(6)) // (1L, 0E)
    )

    assert(chains == expectedWebs)
  }
}
