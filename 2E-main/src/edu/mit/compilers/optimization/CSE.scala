package edu.mit.compilers.optimization

import scala.collection.mutable.{Map => MutableMap}
import edu.mit.compilers.codegen._

// NOTE: we may want to differentiate between global and local versions
// of these optimizations.

// Common Subexpression Elimination
case object CSE extends Optimization {
  val varToVals: MutableMap[BasicBlock, Map[Register, String]] = MutableMap()
  val exprToVals: MutableMap[BasicBlock, Map[String, String]] = MutableMap()
  val exprToTmps: MutableMap[BasicBlock, Map[String, Register]] = MutableMap()
  val tmpToExprs: MutableMap[BasicBlock, Map[Register, String]] = MutableMap()
  val seenArrElemsMap: MutableMap[BasicBlock, Set[ArrElemAddr]] = MutableMap()

  def isGlobal(reg: Register): Boolean = reg
    .isInstanceOf[Addr] && reg.asInstanceOf[Addr].location == AddrLocation.Data

  def isVar(reg: Register): Boolean = reg.isInstanceOf[ScalarAddr] && reg
    .asInstanceOf[ScalarAddr]
    .varNameOpt
    .isDefined

  def cse(bb: BasicBlock): BasicBlock = {
    var globalArgs: Set[Register] = Set()
    // intersect the keys and values of maps from all parents
    var varToVal = intersect[Register, String](bb.parents.map(varToVals))
    var exprToVal = intersect[String, String](bb.parents.map(exprToVals))
    var exprToTmp = intersect[String, Register](bb.parents.map(exprToTmps))
    var tmpToExpr = intersect[Register, String](bb.parents.map(tmpToExprs))
    var seenArrElems: Set[ArrElemAddr] =
      if (bb.parents.isEmpty) Set()
      else bb.parents.map(seenArrElemsMap).reduce((acc, cur) => acc & cur)

    var valCounter = varToVal.size + exprToVal.size

    def valFromArg(arg: Register): String = {
      if (isVar(arg)) {
        varToVal += (arg -> s"v${valCounter}")
        valCounter += 1
        varToVal(arg)
      } else {
        // if its a tmp, check if the tmp is an expr we've seen before
        exprToVal.getOrElse(tmpToExpr.getOrElse(arg, arg.name), arg.name)
      }
    }

    def isArrElem(reg: Register): Boolean = reg match {
      case arrElemAddr: ArrElemAddr => {
        seenArrElems += arrElemAddr
        true
      }
      case _ => {
        false
      }
    }

    def valFromExpr(expr: String): String = {
      exprToVal += (expr -> s"v${varToVal.size + exprToVal.size}")
      valCounter += 1
      exprToVal(expr)
    }

    bb.instrs = bb.instrs.map {
      case binOp: BinOpInstr => {
        // Don't CSE this inst if it contains array elements
        // pulled out to prevent short circuiting on this condition
        val isArg1ArrElem = isArrElem(binOp.arg1)
        val isArg2ArrElem = isArrElem(binOp.arg2)
        if (
          (isArg1ArrElem && !binOp.arg1
            .asInstanceOf[ArrElemAddr]
            .index
            .isInstanceOf[IntConstReg]) || (isArg2ArrElem && !binOp.arg2
            .asInstanceOf[ArrElemAddr]
            .index
            .isInstanceOf[IntConstReg])
        ) {
          binOp
        } else {
          // get variable values or initialize
          // variable values to a counter
          val arg1Val = varToVal.getOrElse(binOp.arg1, valFromArg(binOp.arg1))
          val arg2Val = varToVal.getOrElse(binOp.arg2, valFromArg(binOp.arg2))
          if (isGlobal(binOp.arg1)) globalArgs += binOp.arg1
          if (isGlobal(binOp.arg2)) globalArgs += binOp.arg2
          // get expr value or initialize
          // the expr value to a counter
          val expr = s"${arg1Val} ${binOp.opStr} ${arg2Val}"
          val reverseExpr = s"${arg2Val} ${binOp.opStr} ${arg1Val}"
          var alreadySeen = true
          val exprVal = binOp match {
            case c: CommInstr =>
              exprToVal.getOrElse(
                expr,
                exprToVal.getOrElse(
                  reverseExpr, {
                    alreadySeen = false
                    exprToTmp += (expr -> binOp.dest)
                    valFromExpr(expr)
                  }
                )
              )
            case n: NonCommInstr =>
              exprToVal.getOrElse(
                expr, {
                  alreadySeen = false
                  exprToTmp += (expr -> binOp.dest)
                  valFromExpr(expr)
                }
              )
          }
          if (isVar(binOp.dest)) {
            varToVal += (binOp.dest -> exprVal)
            seenArrElems =
              seenArrElems.filter(arrElem => arrElem.index != binOp.dest)
          }
          // regardless of whether we've seen the expr before,
          // we want to remember this tmp has this expr
          tmpToExpr += (binOp.dest -> expr)
          if (!alreadySeen) {
            binOp
          } else {
            CopyInstr(
              binOp.dest,
              exprToTmp.getOrElse(expr, exprToTmp(reverseExpr))
            )
          }
        }
      }
      case unOp: UnOpInstr => {
        if (
          isArrElem(unOp.arg) && !unOp.arg
            .asInstanceOf[ArrElemAddr]
            .index
            .isInstanceOf[IntConstReg]
        ) {
          unOp
        } else {
          val argVal = valFromArg(unOp.arg)
          if (isGlobal(unOp.arg)) globalArgs += unOp.arg
          val expr = s"${unOp.opStr} ${argVal}"
          var alreadySeen = true
          val exprVal = exprToVal.getOrElse(
            expr, {
              alreadySeen = false
              exprToTmp += (expr -> unOp.dest)
              valFromExpr(expr)
            }
          )
          if (isVar(unOp.dest)) {
            varToVal += (unOp.dest -> exprVal)
            seenArrElems =
              seenArrElems.filter(arrElem => arrElem.index != unOp.dest)
          }
          tmpToExpr += (unOp.dest -> expr)
          if (!alreadySeen) {
            unOp
          } else {
            CopyInstr(unOp.dest, exprToTmp(expr))
          }
        }
      }
      case call: CallInstr => {
        // if a call instr is called
        // we gotta forget all exprs that have globals
        val globalVars = varToVal.filterKeys(v => isGlobal(v))
        varToVal --= globalVars.keySet
        exprToVal = exprToVal.filterKeys(expr =>
          !expr
            .split(" ")
            .exists(v => globalVars.valuesIterator.contains(v))
        )
        exprToTmp = exprToTmp.filterKeys(expr =>
          !expr
            .split(" ")
            .exists(v => globalVars.valuesIterator.contains(v))
        )
        tmpToExpr =
          tmpToExpr.filterKeys(tmp => exprToTmp.valuesIterator.contains(tmp))
        seenArrElems = seenArrElems.filter(arrElem => !isVar(arrElem.index))
        if (isVar(call.dest)) {
          varToVal += (call.dest -> s"v${valCounter}")
          valCounter += 1
        }
        call
      }
      case copy: CopyInstr => {
        // if a variable is written to
        // update its value
        if (isVar(copy.dest)) {
          varToVal += (copy.dest -> s"v${valCounter}")
          valCounter += 1
          seenArrElems =
            seenArrElems.filter(arrElem => arrElem.index != copy.dest)
        }
        copy
      }
      case arrCheck: ArrCheckInstr => {
        if (seenArrElems.contains(arrCheck.arrElem)) EmptyInstr
        else arrCheck
      }
      case instr => instr
    }
    varToVals(bb) = varToVal
    exprToVals(bb) = exprToVal
    exprToTmps(bb) = exprToTmp
    tmpToExprs(bb) = tmpToExpr
    seenArrElemsMap(bb) = seenArrElems
    bb
  }
  override def opt(cfg: BasicBlock): BasicBlock = {
    assert(
      cfg.functionHeader.getOrElse(
        throw new Exception("Provide function header to `CSE.opt`")
      ) == cfg,
      "Need to be given function header (CFG) to `CSE.opt`"
    )

    // Initialize each of the maps
    val bbs: List[BasicBlock] = cfg.visit[BasicBlock](x => x)
    bbs.foreach(bb => varToVals(bb) = Map())
    bbs.foreach(bb => exprToVals(bb) = Map())
    bbs.foreach(bb => exprToTmps(bb) = Map())
    bbs.foreach(bb => tmpToExprs(bb) = Map())
    bbs.foreach(bb => seenArrElemsMap(bb) = Set())

    // Do CSE
    cfg.visit[BasicBlock](cse)
    cfg
  }
  override def toString(): String = "cse"

  override def precedence(): Int = Precedences.CSE
}
