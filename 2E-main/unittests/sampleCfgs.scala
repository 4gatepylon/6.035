// https://docs.scala-lang.org/tour/packages-and-imports.html
// Scala is agnostic to file layout
package unittests

import edu.mit.compilers.parser._
import edu.mit.compilers.semantics._
import edu.mit.compilers.codegen._

// NOTE that paramFx is different from the rest in that it doesn't return everything.
// However, every function returns `(List[BasicBlock], List[Register])`
// In the future we sort of need a CFG creator.
case object SampleCFGs {
  // Tables that are not particularly important, but mainly used as boilerplate
  val globalTable = new RegTable(parent = None)
  val emptyParamTable = new RegTable(parent = Some(globalTable))
  val emptyDummyVarTable = new RegTable(parent = Some(emptyParamTable))

  // Boilerplate that is commonly reused
  val astTrueExpr: ASTExpr = ASTBoolLiteral(BoolToken("true", 1, 3))
  val astIfCondStmt: ASTCondStmt = ASTIfThenStmt(
    Keyword("if", 1, 1),
    ASTBoolLiteral(BoolToken("true", 1, 3)),
    ASTBlock(Mark("{", 2, 1), List(), List())
  )
  val astWhileCondStmt: ASTCondStmt = ASTWhileStmt(
    Keyword("while", 1, 1),
    ASTBoolLiteral(BoolToken("true", 1, 3)),
    ASTBlock(Mark("{", 2, 1), List(), List())
  )

  def smallSingletonFx(): (List[BasicBlock], List[Register]) = {
    // NOTE this is copied from the `defUse.scala` unit tests
    val bb: BasicBlock = BlockMaker.seq(List(), None, 0, None)
    val reg1: Register = RegMaker.tmpScalarStackReg()
    val reg2: Register = RegMaker.tmpScalarStackReg()
    // Test hardcoded flatten
    bb.instrs = List(
      CopyInstr(reg1, reg2),
      AddInstr(reg1, reg2, reg2),
      CopyInstr(reg2, reg1)
    )

    bb.functionHeader = Some(bb)
    bb.regTable = Some(emptyDummyVarTable)
    bb.instrs.foreach(instr => instr.block = Some(bb))

    (List(bb), List(reg1, reg2))
  }

  def bigSingletonFx(): (List[BasicBlock], List[Register]) = {
    // This tests the single basic block case:
    // Tests the following
    // - Ensure that def always comes before use if there are no loops
    // - Ensure that constants are not def-used
    // - Explore case with multiple uses per line, single def per line with one no op
    //   (no-op means that the line numbers jump at a certain point)

    val bb: BasicBlock = BlockMaker.seq(
      /* We won't use flatten so this doesn't matter */
      List(),
      /* No children. */
      None,
      /* Top scope. */
      0,
      /* No loop. */
      None
    )

    val reg1: Register = RegMaker.tmpScalarStackReg()
    val reg2: Register = RegMaker.tmpScalarStackReg()
    val reg3: Register = RegMaker.tmpScalarStackReg()

    val three = IntConstReg(3L)
    val four = IntConstReg(4L)

    // Test hardcoded flatten
    bb.instrs = List(
      /* (0) Definition of reg1, use of reg2, reg3 */
      AddInstr(reg1, reg2, reg3),
      /* (1) Definition of reg1. */
      AddInstr(reg1, three, four),
      /* (2) Definition of reg3, use of reg1. */
      CopyInstr(reg3, reg1),
      /* (3) Definition of reg2, use of reg1. */
      CopyInstr(reg2, reg1),
      /* (4) Nothing is defined */
      NoOpInstr(),
      /* (5) Definition of reg3, use of reg1, reg2, and reg3. */
      CallInstr("func", reg3, List(reg1, reg2, reg3))
    )
    // We'll set this as the header for now
    bb.functionHeader = Some(bb)
    bb.regTable = Some(emptyDummyVarTable)
    bb.instrs.foreach(instr => instr.block = Some(bb))

    (List(bb), List(reg1, reg2, reg3))
  }

  def forkFx(): (List[BasicBlock], List[Register]) = {
    // This just makes sure that the def-use works on a DAG with multiple
    // blocks with both splits and merges.
    val end: BasicBlock = BlockMaker.seq(
      /* We won't use flatten so this doesn't matter */
      List(),
      /* End of the CFG. */
      None,
      /* No scope. */
      0,
      /* No Loop. */
      None
    )
    val fork: ForkBlock = BlockMaker.fork(
      /* We won't use flatten so these don't matter */
      astTrueExpr,
      astIfCondStmt,
      /* True child, false child (changes later). */
      end,
      end,
      /* Top scope. */
      0,
      /* No loop. */
      None
    )
    val body: BasicBlock = BlockMaker.seq(
      /* We won't use flatten so this doesn't matter */
      List(),
      /* Child: diamond. */
      Some(end),
      /* If scope. */
      1,
      /* No Loop. */
      None
    )

    fork.trueChild = body
    fork.falseChild = end
    fork.children = List(body, end)
    body.parents = List(fork)
    end.parents = List(fork, body)

    val reg1: Register = RegMaker.tmpScalarStackReg()
    val reg2: Register = RegMaker.tmpScalarStackReg()

    fork.condDest = Option(reg1)
    fork.instrs = List(
      /* (0) Definition of reg1 */
      AndInstr(reg1, reg1, reg2)
    )
    body.instrs = List(
      /* (0) Definition of reg2, use of reg1 */
      CopyInstr(reg2, reg1),
      /* (1) Nothing is defined */
      NoOpInstr()
    )
    end.instrs = List(
      /* (0) Definition of reg2, use of reg1 */
      CopyInstr(reg1, reg2),
      /* (1) Nothing is defined */
      NoOpInstr(),
      /* (2) Definition of reg2, use of reg1 */
      CopyInstr(reg2, reg1)
    )

    // Make sure that the parameter tables and related data are populated
    val cfgHeader: BasicBlock = BlockMaker.seq(List(), Some(fork), 0, None)
    fork.parents = List(cfgHeader)
    cfgHeader.regTable = Some(emptyDummyVarTable)
    fork.regTable = Some(emptyDummyVarTable)
    body.regTable = Some(new RegTable(parent = fork.regTable))
    end.regTable = Some(emptyDummyVarTable)
    List(cfgHeader, fork, body, end).foreach(bb => {
      bb.functionHeader = Some(cfgHeader)
      bb.instrs.foreach(instr => instr.block = Some(bb))
    })

    assert(fork.trueChild == body)
    assert(fork.falseChild == end)
    assert(body.parents == List(fork))
    assert(end.parents == List(fork, body))
    assert(fork.parents == List(cfgHeader))

    (List(cfgHeader, fork, body, end), List(reg1, reg2))
  }

  def nestedForkFx(): (List[BasicBlock], List[Register]) = {
    // NOTE we will want a compositional framework for this in the future
    val (inner_bbs, inner_regs) = forkFx()
    val (outer_bbs, outer_regs) = forkFx()
    assert(
      inner_bbs.size == 4 && inner_regs.size == 2 && outer_bbs.size == 4 && outer_regs.size == 2
    )
    // We ignore innerForkCFG header and outerBody since outerBody is replaced by
    // innerFork followed by innerBody and innerEnd and innerForkCFG is just
    // replaced by innerFork.
    val (_, innerFork, innerBody, innerEnd) =
      (inner_bbs(0), inner_bbs(1), inner_bbs(2), inner_bbs(3))
    val (outerForkCfgHeader, outerFork, _, outerEnd) =
      (outer_bbs(0), outer_bbs(1), outer_bbs(2), outer_bbs(3))

    // Update the function header
    (inner_bbs ++ outer_bbs).foreach(bb =>
      bb.functionHeader = Some(outerForkCfgHeader)
    )

    // NOTE we do not fix scope depths since they are vals
    // and they don't really... matter for the algorithms we use
    // (same as how we don't make sure the registers are shared).

    // Fix the pointers for the outer fork
    outerFork.asInstanceOf[ForkBlock].trueChild = innerFork
    outerFork.asInstanceOf[ForkBlock].falseChild = outerEnd
    outerFork.children = List(innerFork, outerEnd)

    // Fix the pointers for the inner fork
    innerFork.parents = List(outerFork)

    // Fix the pointers for inner end
    innerEnd.asInstanceOf[SeqBlock].child = Some(outerEnd)
    innerEnd.children = List(outerEnd)

    // Fix the outer end pointers
    outerEnd.parents = List(outerFork, innerEnd)

    // Fix the register table for the nested if statement
    // (noting that the outer is already just a table right under the
    // param table and then OK)
    innerFork.regTable = Some(new RegTable(parent = outerFork.regTable))
    innerBody.regTable = Some(new RegTable(parent = innerFork.regTable))
    innerEnd.regTable = innerFork.regTable

    // Sanity test to make sure that we have the right nested diamond graph structure
    assert(outerForkCfgHeader.asInstanceOf[SeqBlock].child == Some(outerFork))
    assert(
      outerFork
        .asInstanceOf[ForkBlock]
        .trueChild == innerFork && outerFork
        .asInstanceOf[ForkBlock]
        .falseChild == outerEnd
    )
    assert(
      innerFork
        .asInstanceOf[ForkBlock]
        .trueChild == innerBody &&
        innerFork
          .asInstanceOf[ForkBlock]
          .falseChild == innerEnd
    )
    assert(innerEnd.asInstanceOf[SeqBlock].child == Some(outerEnd))
    assert(outerEnd.asInstanceOf[SeqBlock].child == None)

    // Sanity test to make sure that the children and parents are correct
    // based on what we saw above
    assert(
      outerForkCfgHeader.parents.size == 0 && outerForkCfgHeader.children == List(
        outerFork
      )
    )
    assert(
      outerFork.parents == List(
        outerForkCfgHeader
      ) && outerFork.children == List(innerFork, outerEnd)
    )
    assert(
      innerFork.parents == List(outerFork) && innerFork.children == List(
        innerBody,
        innerEnd
      )
    )
    assert(
      innerBody.parents == List(innerFork) && innerBody.children == List(
        innerEnd
      )
    )
    assert(
      innerEnd.parents == List(
        innerFork,
        innerBody
      ) && innerEnd.children == List(outerEnd)
    )
    assert(
      outerEnd.parents == List(
        outerFork,
        innerEnd
      ) && outerEnd.children == List()
    )

    // Make sure that the regTable parents are OK
    // (get will fail naturally if there is that sort of error)
    assert(outerFork.regTable == outerForkCfgHeader.regTable)
    assert(innerFork.regTable.get.parent == outerFork.regTable)
    assert(innerBody.regTable.get.parent == innerFork.regTable)
    assert(innerEnd.regTable == innerFork.regTable)
    assert(outerEnd.regTable == outerFork.regTable)

    // Return only the used bbs and regs
    // NOTE that the regs are completely unrelated between the inner
    // and outer fork. We can change that later, but for now it's ok
    // since this is mainly useful for linear allocation.
    val bbs = List(
      outerForkCfgHeader,
      outerFork,
      innerFork,
      innerBody,
      innerEnd,
      outerEnd
    )
    val regs = List(inner_regs(0), inner_regs(1), outer_regs(0), outer_regs(1))
    (bbs, regs)
  }

  def paramFx(): (List[BasicBlock], List[Register]) = {
    val decaf: String = """
    int foo(int a) {
      return a;
    }
    void main() {
      foo(0);
    }
    """

    // Scanner has debug set to false because we design the decaf string
    val scanner: Scanner = new Scanner(decaf, false)
    val parser: Parser = new Parser(scanner)
    val root: ASTNode = parser.parse()
    assert(!parser.hasError)

    Pemdas.order(root)
    TableInserter.visit(root, Symbols(root))
    Checker.visit(root)
    assert(Collector.visit(root).isEmpty)

    val manager: CFGManager = new CFGManager(root)
    manager.mkCFGs()

    // At this point it should be unoptimized and
    // there may be temp variables (though in theory not)
    // However, we know that there are:
    // 1. Exactly 1 parameter in foo
    // 2. Exactly 1 return statement in foo
    // Therefore, there should be >= 1 use of the parameter
    // and exactly one def of the parameter (which we try
    // to find with the reg table).
    // We can ignore main.

    assert(manager.cfgs.contains("foo"))
    val cfg: BasicBlock = manager.cfgs("foo")
    val table: RegTable = cfg.regTable.getOrElse(
      throw new Exception(
        "No reg table for foo in test `Def Use Finds Parameters`"
      )
    )
    assert(table.contains("a"))
    val a: Register = table.get("a")

    // Note this may be incomplete, but we don't know exactly what else there will be
    (List(cfg), List(a))
  }

  def loopFx(): (List[BasicBlock], List[Register]) = {
    // The point of this test is to see that if you have a loop and the block is
    // reachable from itself, uses that come before definitions can also be reached
    // by those definitions.
    val end: BasicBlock = BlockMaker.noOp(None, 0, None)

    val header: ForkBlock = BlockMaker.fork(
      /* We won't use flatten so these don't matter */
      astTrueExpr,
      astWhileCondStmt,
      /* True child, false child (changes later). */
      end,
      end,
      /* Top scope. */
      0,
      /* No loop. */
      None
    )

    val loop: BasicBlock = BlockMaker.seq(
      /* We won't use flatten so this doesn't matter */
      List(),
      /* Loop back to header of loop. */
      Some(header),
      /* Loop scope. */
      1,
      /* Loop. */
      Some(header)
    )

    header.trueChild = loop
    header.falseChild = end
    header.children = List(loop, end)
    loop.parents = List(header)
    end.parents = List(header)

    val reg1: Register = RegMaker.tmpScalarStackReg()
    val reg2: Register = RegMaker.tmpScalarStackReg()

    // CFG header is pre-header
    val cfgHeader: BasicBlock = BlockMaker.seq(List(), Some(header), 0, None)
    cfgHeader.instrs = List(
      /* (0) Definition of reg2, use of reg2 */
      CopyInstr(reg1, reg2)
    )
    header.condDest = Option(reg1)
    header.instrs = List(
    )
    loop.instrs = List(
      /* (0) Definition of reg1, use of reg2, reg1 */
      AddInstr(reg1, reg1, reg2),
      /* (1) Definition of reg2, use of reg1. */
      CopyInstr(reg2, reg1),
      /* (2) Nothing is defined */
      NoOpInstr()
    )
    end.instrs = List(
      /* (0) Definition of Reg1, use of reg2 */
      AddInstr(reg1, reg1, reg2)
    )

    // Make sure that register tables are populated for any parameter defs
    header.parents = List(cfgHeader, loop)
    cfgHeader.regTable = Some(emptyDummyVarTable)
    header.regTable = Some(emptyDummyVarTable)
    loop.regTable = Some(new RegTable(parent = header.regTable))
    end.regTable = Some(emptyDummyVarTable)
    List(cfgHeader, header, loop, end).foreach(bb => {
      bb.functionHeader = Some(cfgHeader)
      bb.instrs.foreach(instr => instr.block = Some(bb))
    })

    (List(cfgHeader, header, loop, end), List(reg1, reg2))
  }

}
