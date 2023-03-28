package edu.mit.compilers.parser

import edu.mit.compilers.semantics.{SemanticError, Table, Symbols, ReturnType}

trait ASTNode {
  var parent: Option[ASTNode] = None
  val token: Token
  var table: Table = Symbols(this)
  var errors: List[SemanticError] = List()
  override def toString = Printer.visit(this)
}

// <program> -> <import_decl>* <field_decl>* <method_decl>*
case class ASTProgram(
    val token: Token,
    val imports: List[ASTImportDecl],
    val fields: List[ASTFieldDecl],
    val methods: List[ASTMethodDecl]
) extends ASTNode

// <import_decl> -> import <id> ;
case class ASTImportDecl(val token: Token, val id: ASTIdNode) extends ASTNode

// <param> -> <type> <id>
case class ASTParam(val token: Token, val dtype: ASTTypeNode, val id: ASTIdNode)
    extends ASTNode

// <var_decl> -> <id> | <id> '[' <int_literal> ']'
trait ASTVarDecl extends ASTNode {
  val id: ASTIdNode
}
case class ASTScalarDecl(val token: Token, val id: ASTIdNode) extends ASTVarDecl
case class ASTArrayDecl(
    val token: Token,
    val id: ASTIdNode,
    val size: ASTIntLiteral
) extends ASTVarDecl

// <field_decl> -> <type> {<var_decl>}+, ;
case class ASTFieldDecl(
    val token: Token,
    val dtype: ASTTypeNode,
    val vars: List[ASTVarDecl]
) extends ASTNode {
  assert(vars.size > 0, "ASTFieldDecl must have at least one variable")
}

// <method_decl> -> {<type> | void} <id> ( [<param>+,] ) <block>
trait ASTMethodDecl extends ASTNode {
  val id: ASTIdNode
  val params: List[ASTParam]
  val block: ASTBlock
}
case class ASTTypeMethodDecl(
    val token: TypeToken,
    val dtype: ASTTypeNode,
    val id: ASTIdNode,
    val params: List[ASTParam],
    val block: ASTBlock
) extends ASTMethodDecl
case class ASTVoidMethodDecl(
    val token: Keyword,
    // Might want to make a ASTMethodName for consistency later
    val id: ASTIdNode,
    val params: List[ASTParam],
    val block: ASTBlock
) extends ASTMethodDecl {
  assert(token.string == "void", "ASTVoidMethodDecl must be void")
}

// <block> -> '{' <field_decl>* <stmt>* '}'
case class ASTBlock(
    val token: Mark,
    val fields: List[ASTFieldDecl],
    val stmts: List[ASTStmt]
) extends ASTNode {
  var returnType: ReturnType.Type = ReturnType.void
  var mustReturn: Boolean = false
  assert(token.string == "{", "ASTBlock must start with {")
}

trait ASTStmt extends ASTNode
// <statement> -> <location> <assign_expr> ;
// <assign_expr> -> = <expr>
trait ASTOpChange extends ASTNode {
  var location: ASTLocation
  var expr: ASTExpr
}
trait ASTMutChange extends ASTNode {
  var location: ASTLocation
}
trait ASTOpAssignStmt extends ASTStmt with ASTOpChange
trait ASTMutAssignStmt extends ASTStmt with ASTMutChange
case class ASTEqAssignStmt(
    val token: Token,
    var location: ASTLocation,
    var expr: ASTExpr
) extends ASTOpAssignStmt {
  assert(token.string == "=", "AssignASTStmt must be =")
}
// <assign_expr> -> <compound_assign_op> <expr>
case class ASTAddAssignStmt(
    val token: Token,
    var location: ASTLocation,
    var expr: ASTExpr
) extends ASTOpAssignStmt {
  assert(token.string == "+=", "ASTAddAssignStmt must be +=")
}
case class ASTSubAssignStmt(
    val token: Token,
    var location: ASTLocation,
    var expr: ASTExpr
) extends ASTOpAssignStmt {
  assert(token.string == "-=", "ASTSubAssignStmt must be -=")
}
// <assign_expr> -> <increment>
case class ASTIncrStmt(val token: Token, var location: ASTLocation)
    extends ASTMutAssignStmt {
  assert(token.string == "++", "ASTIncrStmt must be ++")
}
case class ASTDecrStmt(val token: Token, var location: ASTLocation)
    extends ASTMutAssignStmt {
  assert(token.string == "--", "ASTDecrStmt must be --")
}
// <method_name> -> <id>
case class ASTMethodName(val token: Token, val id: ASTIdNode) extends ASTStmt
// <statement> -> <method_call> ;
// <method_call> -> <method_name> ( [<expr>+,] )
trait ASTMethodCall extends ASTNode {
  val name: ASTMethodName
  var args: List[ASTImportArg]
}
case class ASTMethodCallStmt(
    val token: Token,
    val name: ASTMethodName,
    var args: List[ASTImportArg]
) extends ASTStmt
    with ASTMethodCall
// <statement> -> if ( <expr> ) <block> [else <block>]
trait ASTCondStmt extends ASTStmt {
  var cond: ASTExpr
}
trait ASTLoopStmt extends ASTCondStmt {
  val block: ASTBlock
}
trait ASTIfStmt extends ASTCondStmt {
  var cond: ASTExpr
  val thenBlock: ASTBlock
}
case class ASTIfThenStmt(
    val token: Keyword,
    var cond: ASTExpr,
    val thenBlock: ASTBlock
) extends ASTIfStmt {
  assert(token.string == "if", "ASTIfThenStmt must be if")
}
case class ASTIfElseStmt(
    val token: Keyword,
    var cond: ASTExpr,
    val thenBlock: ASTBlock,
    val elseBlock: ASTBlock
) extends ASTIfStmt {
  assert(token.string == "if", "ASTIfElseStmt must be if")
}
// <statement> -> for ( <id> = <expr> ; <expr> ; <for_update> ) <block>
case class ASTForStmt(
    val token: Keyword,
    val id: ASTIdNode,
    var init: ASTExpr,
    var cond: ASTExpr,
    val update: ASTForUpdate,
    val block: ASTBlock
) extends ASTLoopStmt {
  assert(token.string == "for", "ASTForStmt must be for")
}
// <statement> -> while ( <expr> ) <block>
case class ASTWhileStmt(
    val token: Keyword,
    var cond: ASTExpr,
    val block: ASTBlock
) extends ASTLoopStmt {
  assert(token.string == "while", "ASTWhileStmt must be while")
}
// <statement> -> return [<expr>] ;
trait ASTReturnStmt extends ASTStmt
case class ASTVoidReturnStmt(val token: Keyword) extends ASTReturnStmt {
  assert(token.string == "return", "ASTVoidReturnStmt must be return")
}
case class ASTTypeReturnStmt(val token: Keyword, var expr: ASTExpr)
    extends ASTReturnStmt {
  assert(token.string == "return", "ASTTypeReturnStmt must be return")
}
// <statement> -> break ;
case class ASTBreakStmt(val token: Keyword) extends ASTStmt {
  assert(token.string == "break", "ASTBreakStmt must be break")
}
// <statement> -> continue ;
case class ASTContinueStmt(val token: Keyword) extends ASTStmt {
  assert(token.string == "continue", "ASTContinueStmt must be continue")
}

// <for_update> -> <location> {<compound_assign_op> <expr> | <increment>}
trait ASTForUpdate extends ASTNode {
  var location: ASTLocation
}
trait ASTOpForUpdate extends ASTForUpdate with ASTOpChange

trait ASTMutForUpdate extends ASTForUpdate with ASTMutChange

// <for_update> -> <location> <compound_assign_op> <expr>
case class ASTAddForUpdate(
    val token: Token,
    var location: ASTLocation,
    var expr: ASTExpr
) extends ASTOpForUpdate {
  assert(token.string == "+=", "ASTAddForUpdate must be +=")
}
case class ASTSubForUpdate(
    val token: Token,
    var location: ASTLocation,
    var expr: ASTExpr
) extends ASTOpForUpdate {
  assert(token.string == "-=", "ASTSubForUpdate must be -=")
}
// <for_update> -> <location> <increment>
case class ASTIncrForUpdate(val token: Token, var location: ASTLocation)
    extends ASTMutForUpdate {
  assert(token.string == "++", "ASTIncrForUpdate must be ++")
}
case class ASTDecrForUpdate(val token: Token, var location: ASTLocation)
    extends ASTMutForUpdate {
  assert(token.string == "--", "ASTDecrForUpdate must be --")
}

trait ASTExpr extends ASTNode with ASTImportArg
// <expr> -> <location>
trait ASTLocation extends ASTExpr {
  val id: ASTIdNode
}
// <location> -> <id> | <id> '[' <expr> ']'
case class ASTScalarLocation(val token: IdToken, val id: ASTIdNode)
    extends ASTLocation
case class ASTArrayLocation(
    val token: IdToken,
    val id: ASTIdNode,
    var index: ASTExpr
) extends ASTLocation
// <expr> -> <method_call>
// <method_call> -> <method_name> ( [<expr>+,] )
case class ASTMethodCallExpr(
    val token: Token,
    val name: ASTMethodName,
    var args: List[ASTImportArg]
) extends ASTExpr
    with ASTMethodCall
// <literal> -> <int_literal> | <char_literal> | <bool_literal>
trait ASTLiteral extends ASTExpr
case class ASTIntLiteral(val token: IntToken) extends ASTLiteral {
  // should only call eval after passing semantic checks
  def eval(): Long = {
    val s = token.string
    if (s.length > 2 && s.take(2) == "0x") {
      BigInt(s.substring(2), 16).toLong
    } else {
      BigInt(s).toLong
    }
  }
}
case class ASTCharLiteral(val token: CharToken) extends ASTLiteral
case class ASTBoolLiteral(val token: BoolToken) extends ASTLiteral
// <import_arg> -> <expr> | <string_literal>
trait ASTImportArg extends ASTNode
case class ASTStringLiteral(val token: StrToken) extends ASTImportArg
// <expr> -> len ( <id> )
case class ASTLenExpr(val token: Keyword, val id: ASTIdNode) extends ASTExpr {
  assert(token.string == "len", s"$token is not len")
}

// This is used for pemdas
trait ASTOpExpr extends ASTExpr

// <expr> -> <expr> <bin_op> <expr>
trait ASTBinOpExpr extends ASTOpExpr {
  var left: ASTExpr
  var right: ASTExpr
}
// <arith_op> -> + | - | * | / | %
trait ASTArithOpExpr extends ASTBinOpExpr
case class ASTAddExpr(
    val token: ArithOp,
    var left: ASTExpr,
    var right: ASTExpr
) extends ASTArithOpExpr {
  assert(token.string == "+", "ASTAddExpr must be +")
}
case class ASTSubExpr(
    val token: ArithOp,
    var left: ASTExpr,
    var right: ASTExpr
) extends ASTArithOpExpr {
  assert(token.string == "-", "ASTSubExpr must be -")
}
case class ASTMulExpr(val token: ArithOp, var left: ASTExpr, var right: ASTExpr)
    extends ASTArithOpExpr {
  assert(token.string == "*", "ASTMulExpr must be *")
}
case class ASTDivExpr(val token: ArithOp, var left: ASTExpr, var right: ASTExpr)
    extends ASTArithOpExpr {
  assert(token.string == "/", "ASTDivExpr must be /")
}
case class ASTModExpr(val token: ArithOp, var left: ASTExpr, var right: ASTExpr)
    extends ASTArithOpExpr {
  assert(token.string == "%", "ASTModExpr must be %")
}
// <rel_op> -> < | > | <= | >=
trait ASTRelOpExpr extends ASTBinOpExpr
case class ASTLtExpr(val token: RelOp, var left: ASTExpr, var right: ASTExpr)
    extends ASTRelOpExpr {
  assert(token.string == "<", "ASTLtExpr must be <")
}
case class ASTGtExpr(val token: RelOp, var left: ASTExpr, var right: ASTExpr)
    extends ASTRelOpExpr {
  assert(token.string == ">", "ASTGtExpr must be >")
}
case class ASTLeExpr(val token: RelOp, var left: ASTExpr, var right: ASTExpr)
    extends ASTRelOpExpr {
  assert(token.string == "<=", "ASTLeExpr must be <=")
}
case class ASTGeExpr(val token: RelOp, var left: ASTExpr, var right: ASTExpr)
    extends ASTRelOpExpr {
  assert(token.string == ">=", "ASTGeExpr must be >=")
}
// <eq_op> -> == | !=
trait ASTEqOpExpr extends ASTBinOpExpr
case class ASTEqExpr(val token: EqOp, var left: ASTExpr, var right: ASTExpr)
    extends ASTEqOpExpr {
  assert(token.string == "==", "ASTEqExpr must be ==")
}
case class ASTNeExpr(val token: EqOp, var left: ASTExpr, var right: ASTExpr)
    extends ASTEqOpExpr {
  assert(token.string == "!=", "ASTNeExpr must be !=")
}
// <cond_op> -> && | ||
trait ASTCondOpExpr extends ASTBinOpExpr
case class ASTAndExpr(val token: CondOp, var left: ASTExpr, var right: ASTExpr)
    extends ASTCondOpExpr {
  assert(token.string == "&&", "ASTAndExpr must be &&")
}
case class ASTOrExpr(val token: CondOp, var left: ASTExpr, var right: ASTExpr)
    extends ASTCondOpExpr {
  assert(token.string == "||", "ASTOrExpr must be ||")
}
// <expr> -> - <expr>
trait ASTUnOpExpr extends ASTOpExpr {
  var expr: ASTExpr
}
case class ASTNegExpr(val token: ArithOp, var expr: ASTExpr)
    extends ASTUnOpExpr {
  assert(token.string == "-", "ASTNegExpr must be -")
}
// <expr> -> ! <expr>
case class ASTNotExpr(val token: LogOp, var expr: ASTExpr) extends ASTUnOpExpr {
  assert(token.string == "!", "ASTNotExpr must be !")
}
// <expr> -> ( <expr> )
case class ASTParenExpr(val token: Mark, var expr: ASTExpr) extends ASTExpr {
  assert(token.string == "(", "ASTParenExpr must be (")
}

case class ASTTypeNode(val token: TypeToken) extends ASTNode

case class ASTIdNode(val token: IdToken) extends ASTNode
