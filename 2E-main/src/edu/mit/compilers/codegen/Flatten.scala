package edu.mit.compilers.codegen

import edu.mit.compilers.parser._

// Example of flattened code
// a = (a + b)*c;

// a -> %0
// b -> %1
// c -> %2

// %3 = %0 + %1
// %4 = %2 // we can skip this step
// %5 = %3 * %4
// %0 = %5

object Flattener {
  def cfg(bb: BasicBlock): Unit = bb.lineage.foreach(block)

  def block(bb: BasicBlock): Unit = {
    val tbl = bb.regTable.getOrElse(
      throw new Exception("Found BB with no table in `Flattener.block`")
    )
    for (n <- bb.nodes.filterNot(_.isInstanceOf[ASTFieldDecl])) {
      // NOTE: there should be no output
      val (instrs, output): (List[BasicInstr], Register) = tree(n, tbl)
      bb.instrs ++= instrs
      if (bb.isInstanceOf[ForkBlock]) {
        bb.asInstanceOf[ForkBlock].condDest = Some(output)
      }
    }
  }

  def tree(node: ASTNode, regTable: RegTable): (List[BasicInstr], Register) = {
    node match {
      case opAssignStmt: ASTOpChange => {
        var instrs = List[BasicInstr]()
        val (locInstrs, destReg) = tree(opAssignStmt.location, regTable)
        instrs ++= locInstrs
        val (exprInstrs, exprReg) = tree(opAssignStmt.expr, regTable)
        instrs ++= exprInstrs
        instrs :+= (opAssignStmt match {
          case _: ASTEqAssignStmt =>
            CopyInstr(destReg, exprReg)
          case _: ASTAddAssignStmt | _: ASTAddForUpdate =>
            AddInstr(destReg, destReg, exprReg)
          case _: ASTSubAssignStmt | _: ASTSubForUpdate =>
            SubInstr(destReg, destReg, exprReg)
          case _ =>
            throw new Exception(s"Unknown opAssignStmt ${opAssignStmt}")
        })
        (instrs, destReg)
      }
      case mutAssignStmt: ASTMutChange => {
        var instrs = List[BasicInstr]()
        val (destInstrs, destReg) = tree(mutAssignStmt.location, regTable)
        instrs ++= destInstrs
        instrs :+= (mutAssignStmt match {
          case _: ASTIncrStmt | _: ASTIncrForUpdate =>
            IncrInstr(destReg, destReg)
          case _: ASTDecrStmt | _: ASTDecrForUpdate =>
            DecrInstr(destReg, destReg)
          case _ =>
            throw new Exception(s"Unknown mutAssignStmt ${mutAssignStmt}")
        })
        (instrs, destReg)
      }
      case methodCall: ASTMethodCall => {
        var instrs = List[BasicInstr]()
        var argRegs: List[Register] =
          methodCall.args.map((arg: ASTImportArg) => {
            val (argInstrs, argReg) = tree(arg, regTable)
            instrs ++= argInstrs
            argReg match {
              case p: ParamReg => {
                val destReg = RegMaker.tmpScalarStackReg()
                instrs ++= List(CopyInstr(destReg, p))
                destReg
              }
              case _ => argReg
            }
          })
        val name = methodCall.name.id.token.string
        // NOTE: this is kinda hacky but idk how else to handle
        // the fact that printf has optional parameters
        // that come before not-optional ones
        if (name == "printf" && methodCall.args.length == 1) {
          // TODO: why do we need StrFormat and IntFormat registers?
          val formatReg: Register = methodCall.args(0) match {
            case ASTStringLiteral(token) => StrFormat
            case ASTIntLiteral(token)    => IntFormat
            case reg =>
              throw new Exception(s"Unknown formatReg $reg")
          }
          argRegs = formatReg +: argRegs
        }
        val destReg: Register = RegMaker.tmpScalarStackReg()
        instrs :+= CallInstr(name, destReg, argRegs)
        (instrs, destReg)
      }
      case ASTLenExpr(token, id) => {
        var instrs = List[BasicInstr]()
        val (idInstrs, idReg) = tree(id, regTable)
        instrs ++= idInstrs
        val destReg: Register = RegMaker.tmpScalarStackReg()
        instrs :+= LenInstr(destReg, idReg)
        (instrs, destReg)
      }
      case binOpExpr: ASTBinOpExpr => {
        var instrs = List[BasicInstr]()
        val (leftInstrs, leftReg) = tree(binOpExpr.left, regTable)
        instrs ++= leftInstrs
        val (rightInstrs, rightReg) = tree(binOpExpr.right, regTable)
        instrs ++= rightInstrs
        val destReg: Register = RegMaker.tmpScalarStackReg()
        instrs :+= InstrMaker.binOpInstr(
          binOpExpr,
          destReg,
          leftReg,
          rightReg
        )
        (instrs, destReg)
      }
      case unOpExpr: ASTUnOpExpr => {
        var instrs = List[BasicInstr]()
        val (exprInstrs, exprReg) = tree(unOpExpr.expr, regTable)
        instrs ++= exprInstrs
        val destReg: Register = RegMaker.tmpScalarStackReg()
        instrs :+= InstrMaker.unOpInstr(unOpExpr, destReg, exprReg)
        (instrs, destReg)
      }
      case ASTVoidReturnStmt(token) =>
        (
          List(RetInstr(RegMaker.tmpBuiltInReg(RaxLoc))),
          RegMaker.tmpBuiltInReg(RaxLoc)
        )
      case ASTTypeReturnStmt(token, expr) => {
        var instrs = List[BasicInstr]()
        val (exprInstrs, exprReg) = tree(expr, regTable)
        instrs ++= exprInstrs
        val destReg = exprReg
        instrs :+= RetInstr(destReg)
        (instrs, destReg)
      }
      // TODO the break instruction may or may not be used
      case ASTBreakStmt(token)       => (List(), RegMaker.tmpBuiltInReg(RaxLoc))
      case ASTContinueStmt(token)    => (List(), RegMaker.tmpBuiltInReg(RaxLoc))
      case ASTParenExpr(token, expr) => tree(expr, regTable)
      case literal: ASTLiteral       => (List(), RegMaker.litReg(literal))
      case ASTStringLiteral(token) =>
        (List(), RegMaker.strReg(token.string))
      case ASTScalarLocation(token, id) =>
        (List(), regTable.get(id.token.string))
      case ASTArrayLocation(token, id, index) => {
        var instrs = List[BasicInstr]()
        val baseReg: Register = regTable.get(id.token.string)
        val (indexInstrs, indexReg) = tree(index, regTable)
        instrs ++= indexInstrs
        val destReg: Register = RegMaker.tmpArrayIndexReg(
          baseReg.asInstanceOf[ArrBaseAddr],
          indexReg,
          baseReg.asInstanceOf[ArrBaseAddr].location
        )
        instrs :+= ArrCheckInstr(destReg.asInstanceOf[ArrElemAddr])
        (instrs, destReg)
      }
      case ASTIdNode(token) => (List(), regTable.get(token.string))
      case n => throw new Exception(s"Unknown node type in flattening: $n")
    }
  }
}
