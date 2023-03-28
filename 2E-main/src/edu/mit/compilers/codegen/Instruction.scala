package edu.mit.compilers.codegen

import edu.mit.compilers.parser._

trait BasicInstr {
  var block: Option[BasicBlock] = None
  def toString: String
  def regs: List[Register] = {
    val dests = this match {
      case d: DestInstr => List(d.dest)
      case _            => List()
    }
    val args = this match {
      case a: ArgsInstr =>
        a.argList
          .map(arg =>
            arg match {
              case ArrElemAddr(base, index, location, id) =>
                arg.argList ++ base.argList ++ index.argList
              case _ => List(arg)
            }
          )
          .flatten
      case _ => List()
    }
    dests ++ args
  }
}

trait DestInstr extends BasicInstr {
  var dest: Register
}

trait ArgsInstr extends BasicInstr {
  def argList: List[Register]
}

case object EmptyInstr extends BasicInstr {
  override def toString: String = ""
}

// Arrows are gotos/jumps (NOT calls for our purposes)
trait BinOpInstr extends DestInstr with ArgsInstr {
  var arg1: Register
  var arg2: Register
  def argList: List[Register] = List(arg1, arg2)
    .map(arg =>
      arg match {
        case ArrElemAddr(base, index, location, id) =>
          arg.argList ++ base.argList ++ index.argList
        case _ =>
          List(arg)
      }
    )
    .flatten
  val opStr: String
  override def toString = s"$dest = $arg1 $opStr $arg2"
}
trait UnOpInstr extends DestInstr with ArgsInstr {
  var arg: Register
  def argList: List[Register] = arg match {
    case ArrElemAddr(base, index, location, id) =>
      arg.argList ++ base.argList ++ index.argList
    case _ =>
      List(arg)
  }
  val opStr: String
  override def toString = s"$dest = $opStr $arg"
}

trait ArithInstr extends BinOpInstr {}
trait CmpInstr extends BinOpInstr {}

// Commutative and non-commutative instrs
trait CommInstr extends BinOpInstr {}
trait NonCommInstr extends BinOpInstr {}

// Binary operations: These always take two inputs and output into a NEW register
case class AddInstr(var dest: Register, var arg1: Register, var arg2: Register)
    extends BinOpInstr
    with ArithInstr
    with CommInstr {
  override val opStr = "+"
}
case class SubInstr(var dest: Register, var arg1: Register, var arg2: Register)
    extends BinOpInstr
    with ArithInstr
    with NonCommInstr {
  override val opStr = "-"
}
case class MulInstr(var dest: Register, var arg1: Register, var arg2: Register)
    extends BinOpInstr
    with ArithInstr
    with CommInstr {
  override val opStr = "*"
}
case class DivInstr(var dest: Register, var arg1: Register, var arg2: Register)
    extends BinOpInstr
    with ArithInstr
    with NonCommInstr {
  override val opStr = "/"
}
case class ModInstr(var dest: Register, var arg1: Register, var arg2: Register)
    extends BinOpInstr
    with ArithInstr
    with NonCommInstr {
  override val opStr = "%"
}
case class AndInstr(var dest: Register, var arg1: Register, var arg2: Register)
    extends BinOpInstr
    with ArithInstr
    with CommInstr {
  override val opStr = "&&"
}
case class OrInstr(var dest: Register, var arg1: Register, var arg2: Register)
    extends BinOpInstr
    with ArithInstr
    with CommInstr {
  override val opStr = "||"
}
case class GtInstr(var dest: Register, var arg1: Register, var arg2: Register)
    extends BinOpInstr
    with CmpInstr
    with NonCommInstr {
  override val opStr = ">"
}
case class LtInstr(var dest: Register, var arg1: Register, var arg2: Register)
    extends BinOpInstr
    with CmpInstr
    with NonCommInstr {
  override val opStr = "<"
}
case class GeInstr(var dest: Register, var arg1: Register, var arg2: Register)
    extends BinOpInstr
    with CmpInstr
    with NonCommInstr {
  override val opStr = ">="
}
case class LeInstr(var dest: Register, var arg1: Register, var arg2: Register)
    extends BinOpInstr
    with CmpInstr
    with NonCommInstr {
  override val opStr = "<="
}
case class EqInstr(var dest: Register, var arg1: Register, var arg2: Register)
    extends BinOpInstr
    with CmpInstr
    with CommInstr {
  override val opStr = "=="
}

case class NeInstr(var dest: Register, var arg1: Register, var arg2: Register)
    extends BinOpInstr
    with CmpInstr
    with CommInstr {
  override val opStr = "!="
}

case class LeftShiftInstr(
    var dest: Register,
    var arg1: Register,
    var arg2: Register
) extends BinOpInstr
    with ArithInstr
    with NonCommInstr {
  override val opStr = "<<"
}

case class RightShiftInstr(
    var dest: Register,
    var arg1: Register,
    var arg2: Register
) extends BinOpInstr
    with ArithInstr
    with NonCommInstr {
  override val opStr = ">>"
}

// No-op basic blocks are going to be basic blocks that have a single No-OP instruction
// After basic blocks are merged, we can decide whether to take off or not NoOps. We keep
// them as instructions in case we need to keep NoOp instructions for the purpose of cache-line
// optimization later.
case class NoOpInstr() extends BasicInstr {
  override def toString = "NoOpInstr"
}

// Unary Operations: Take always one input and store the output in a NEW register
case class NegInstr(var dest: Register, var arg: Register) extends UnOpInstr {
  override val opStr = "-"
}
case class NotInstr(var dest: Register, var arg: Register) extends UnOpInstr {
  override val opStr = "!"
}
case class IncrInstr(var dest: Register, var arg: Register) extends UnOpInstr {
  override val opStr = "++"
}
case class DecrInstr(var dest: Register, var arg: Register) extends UnOpInstr {
  override val opStr = "--"
}

case class LenInstr(var dest: Register, var arg: Register) extends UnOpInstr {
  override val opStr = "Len"
}

// Special Operation: Always take one input and store it in a Register that is NOT NEW
case class CopyInstr(var dest: Register, var arg: Register)
    extends DestInstr
    with ArgsInstr {
  def argList: List[Register] = arg match {
    case ArrElemAddr(base, index, location, id) =>
      arg.argList ++ base.argList ++ index.argList
    case _ =>
      List(arg)
  }
  override def toString = s"$dest = $arg"
}

// Call output will always be put in a temp, but sometimes it may be ignored
case class CallInstr(
    val funcName: String,
    var dest: Register,
    var args: List[Register]
) extends DestInstr
    with ArgsInstr {
  def argList: List[Register] = args
    .map(arg =>
      arg match {
        case ArrElemAddr(base, index, location, id) =>
          arg.argList ++ base.argList ++ index.argList
        case _ =>
          List(arg)
      }
    )
    .flatten
  override def toString = {
    val argStr = args.mkString(", ")
    s"$dest = call $funcName($argStr)"
  }
}
case class RetInstr(var arg: Register) extends BasicInstr with ArgsInstr {
  def argList: List[Register] = arg match {
    case ArrElemAddr(base, index, location, id) =>
      arg.argList ++ base.argList ++ index.argList
    case _ =>
      List(arg)
  }
  override def toString = s"return $arg"
}

// Continue: When you hit this, knowing the block id of the loop header
// (that block which is pointed to by the back-edge at the end of this block)
// emit `jmp loop_header_block_id`
case class ContInstr() extends BasicInstr {
  override def toString = s"continue"
}

// Break: When you hit this, knowing the block id of the OTHER block
// pointed to by the loop header (fork), emit `jmp_other_block`
case class BreakInstr() extends BasicInstr {
  override def toString = s"break"
}

// Make an ArgInstr?
case class DeclInstr(val reg: Register, val size: Long = 0) extends BasicInstr {
  override def toString = s"init $reg"
}

case class ArrCheckInstr(val arrElem: ArrElemAddr) extends ArgsInstr {
  override def toString = s"check $arrElem"
  def argList: List[Register] =
    arrElem.argList ++ arrElem.base.argList ++ arrElem.index.argList
}

object InstrMaker {
  def binOpInstr(
      n: ASTBinOpExpr,
      dest: Register,
      arg1: Register,
      arg2: Register
  ): BasicInstr = {
    n match {
      // equivalence
      case _: ASTEqExpr => EqInstr(dest, arg1, arg2)
      case _: ASTNeExpr => NeInstr(dest, arg1, arg2)
      // relational
      case _: ASTLtExpr => LtInstr(dest, arg1, arg2)
      case _: ASTGtExpr => GtInstr(dest, arg1, arg2)
      case _: ASTLeExpr => LeInstr(dest, arg1, arg2)
      case _: ASTGeExpr => GeInstr(dest, arg1, arg2)
      // arithmetic
      case _: ASTAddExpr => AddInstr(dest, arg1, arg2)
      case _: ASTSubExpr => SubInstr(dest, arg1, arg2)
      case _: ASTMulExpr => MulInstr(dest, arg1, arg2)
      case _: ASTDivExpr => DivInstr(dest, arg1, arg2)
      case _: ASTModExpr => ModInstr(dest, arg1, arg2)
      // conditional
      case _: ASTAndExpr => AndInstr(dest, arg1, arg2)
      case _: ASTOrExpr  => OrInstr(dest, arg1, arg2)
      // unsupported
      case _ => throw new Exception(s"Unsupported binary operation: $n")
    }
  }

  def unOpInstr(n: ASTUnOpExpr, dest: Register, arg: Register): BasicInstr = {
    n match {
      // negation
      case _: ASTNegExpr => NegInstr(dest, arg)
      // logical not
      case _: ASTNotExpr => NotInstr(dest, arg)
      // unsupported
      case _ => throw new Exception(s"Unsupported unary operation: $n")
    }
  }
}
