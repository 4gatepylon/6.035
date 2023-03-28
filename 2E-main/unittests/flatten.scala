import org.scalatest.FunSuite

import edu.mit.compilers.codegen._
import edu.mit.compilers.parser._
import edu.mit.compilers.semantics._

class FlattenTester extends FunSuite {
  test("A nested addition") {
    val expr: ASTNode = ASTEqAssignStmt(
      AssOp("=", 0, 2),
      ASTScalarLocation(
        IdToken("a", 0, 0),
        ASTIdNode(IdToken("a", 0, 0))
      ),
      ASTAddExpr(
        ArithOp("+", 0, 8),
        ASTAddExpr(
          ArithOp("+", 0, 4),
          ASTIntLiteral(IntToken("1", 0, 2)),
          ASTIntLiteral(IntToken("2", 0, 6))
        ),
        ASTIntLiteral(IntToken("3", 0, 10))
      )
    )
    val table: RegTable = RegTable()
    table.insert("a", RegMaker.tmpScalarStackReg())
    val instrs: List[BasicInstr] = Flattener.tree(expr, table)._1
    RegAllocator.resetStack()
    assert(true)
  }

  test("Some array stuff") {
    val expr: ASTNode = ASTEqAssignStmt(
      AssOp("=", 0, 9),
      ASTArrayLocation(
        IdToken("a", 0, 0),
        ASTIdNode(IdToken("a", 0, 0)),
        ASTAddExpr(
          ArithOp("+", 0, 4),
          ASTIntLiteral(IntToken("1", 0, 2)),
          ASTIntLiteral(IntToken("2", 0, 6))
        )
      ),
      ASTAddExpr(
        ArithOp("+", 0, 17),
        ASTAddExpr(
          ArithOp("+", 0, 13),
          ASTIntLiteral(IntToken("1", 0, 11)),
          ASTIntLiteral(IntToken("2", 0, 15))
        ),
        ASTMethodCallExpr(
          IdToken("yeet", 0, 19),
          ASTMethodName(
            IdToken("yeet", 0, 19),
            ASTIdNode(IdToken("yeet", 0, 19))
          ),
          List(
            ASTStringLiteral(StrToken("yote", 0, 24))
          )
        )
      )
    )
    val table: RegTable = RegTable()
    table.insert("a", RegMaker.varArrayBaseReg("a", 5, AddrLocation.Stack))
    val instrs: List[BasicInstr] = Flattener.tree(expr, table)._1
    RegAllocator.resetStack()
    assert(true)
  }

  test("BinOpExpr suite") {
    val expr: ASTNode = ASTLtExpr(
      RelOp("<", 0, 10),
      ASTMulExpr(
        ArithOp("*", 0, 6),
        ASTAddExpr(
          ArithOp("+", 0, 2),
          ASTIntLiteral(IntToken("1", 0, 0)),
          ASTIntLiteral(IntToken("2", 0, 4))
        ),
        ASTIntLiteral(IntToken("3", 0, 8))
      ),
      ASTIntLiteral(IntToken("10", 0, 12))
    )
    val table: RegTable = RegTable()
    val instrs: List[BasicInstr] = Flattener.tree(expr, table)._1
    RegAllocator.resetStack()
    assert(true)
  }

  test("A method call") {
    val expr: ASTNode = ASTEqAssignStmt(
      AssOp("=", 0, 2),
      ASTScalarLocation(
        IdToken("a", 0, 0),
        ASTIdNode(IdToken("a", 0, 0))
      ),
      ASTMethodCallExpr(
        IdToken("yeet", 0, 4),
        ASTMethodName(
          IdToken("yeet", 0, 4),
          ASTIdNode(IdToken("yeet", 0, 4))
        ),
        List(
          ASTIntLiteral(IntToken("10", 0, 12)),
          ASTScalarLocation(
            IdToken("b", 0, 0),
            ASTIdNode(IdToken("b", 0, 0))
          ),
          ASTAddExpr(
            ArithOp("+", 0, 2),
            ASTIntLiteral(IntToken("1", 0, 0)),
            ASTIntLiteral(IntToken("2", 0, 4))
          )
        )
      )
    )
    val table: RegTable = RegTable()
    table.insert("a", RegMaker.tmpScalarStackReg())
    table.insert("b", RegMaker.tmpScalarStackReg())
    val instrs: List[BasicInstr] = Flattener.tree(expr, table)._1
    RegAllocator.resetStack()
    assert(true)
  }
}
