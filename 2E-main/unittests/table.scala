import org.scalatest.FunSuite

import edu.mit.compilers.semantics._
import edu.mit.compilers.parser._

class TableTester extends FunSuite {
  test("Just A Program") {
    // println("*********** Empty Program")
    val root = ASTProgram(IdToken("test", 1, 1), List(), List(), List())
    TableInserter.visit(root, Symbols(root, parent = None))
    // println(root)
    // println()
    assert(true)
  }

  test("Program with Import") {
    // println("*********** Program with One Import")
    val root = ASTProgram(
      IdToken("test", 1, 1),
      List(
        ASTImportDecl(Keyword("import", 1, 1), ASTIdNode(IdToken("test", 1, 8)))
      ),
      // No fields
      List(),
      // No methods
      List()
    )
    TableInserter.visit(root, Symbols(root, parent = None))
    // println(root)
    // println()
    assert(true)
  }

  test("Program with main") {
    // println("********** Program with main")
    val root = ASTProgram(
      IdToken("void", 1, 1),
      // No imports
      List(),
      // No fields
      List(),
      // One method
      List(
        ASTVoidMethodDecl(
          Keyword("void", 1, 1),
          ASTIdNode(IdToken("main", 1, 1)),
          // No parameters
          List(),
          ASTBlock(
            Mark("{", 2, 2),
            // No fields
            List(),
            // No statements
            List()
          )
        )
      )
    )

    // println(root)
    // println()
    assert(true)
  }

  // For more rigorous testing just try out
  // `python3 test.py -v -f legal-01 semantics`
  // NOTE: that
  //  -v = verbose (don't swallow STDOUT)
  //  -f = file (put in a glob/regex to match one or more files)
  // legal-01 is a big (for Decaf) program that does quicksort
}
