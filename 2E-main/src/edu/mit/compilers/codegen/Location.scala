package edu.mit.compilers.codegen

// Note that in the future we are going to be replacing
// all references to registers in the "Register" trait
// and instead using them here in the RegLoc (which is
// thusly named right now because we want backwards compatibility
// while we port).
//
// In the future we will probably want a better name for
// "Register" ... maybe something like "Virtual" or "Variable"
// or whatever. Basically a "Register" in the future is going
// to just be a symbol marking where something is stored, while
// a Loc tells you where it will be in memory after allocation.

trait Loc {
  def toString: String
}

trait RegLoc extends Loc

case object RaxLoc extends RegLoc {
  override def toString = "%rax"
}

case object RspLoc extends RegLoc {
  override def toString = "%rsp"
}

case object RbpLoc extends RegLoc {
  override def toString = "%rbp"
}

// 1st Parameter Register
case object RdiLoc extends RegLoc {
  override def toString = "%rdi"
}

// 2nd Parameter Register
case object RsiLoc extends RegLoc {
  override def toString = "%rsi"
}

// 3rd Parameter Register
case object RdxLoc extends RegLoc {
  override def toString = "%rdx"
}

// 4th Parameter Register
case object RcxLoc extends RegLoc {
  override def toString = "%rcx"
}

// 5th Parameter Register
case object R8Loc extends RegLoc {
  override def toString = "%r8"
}

// 6th Parameter Register
case object R9Loc extends RegLoc {
  override def toString = "%r9"
}

case object R10Loc extends RegLoc {
  override def toString = "%r10"
}

case object R11Loc extends RegLoc {
  override def toString = "%r11"
}

case object R12Loc extends RegLoc {
  override def toString = "%r12"
}

case object R13Loc extends RegLoc {
  override def toString = "%r13"
}

case object R14Loc extends RegLoc {
  override def toString = "%r14"
}

case object R15Loc extends RegLoc {
  override def toString = "%r15"
}

object RegLocInfo {
  // We ignore %rip, %rsp, and %rbp because they
  // would be particularly tricky to use in
  // optimizations and any such optimizations are
  // like to not be meaningful anyways
  val allRegLocs: List[RegLoc] = List(
    // Program flow
    RaxLoc,
    RbpLoc,
    RspLoc,
    // Parameters
    RdiLoc,
    RsiLoc,
    RdxLoc,
    RcxLoc,
    R8Loc,
    R9Loc,
    // Callee saved
    R10Loc,
    R11Loc,
    R12Loc,
    R13Loc,
    R14Loc,
    R15Loc
  )

  val paramRegLocs: List[RegLoc] = List(
    RdiLoc,
    RsiLoc,
    RdxLoc,
    RcxLoc,
    R8Loc,
    R9Loc
  )

  // https://wiki.osdev.org/CPU_Registers_x86-64
  // For more info look at this ^
  val allRegLocsSet = allRegLocs.toSet
  val paramRegLocsSet = paramRegLocs.toSet

  def isBuiltIn(reg: Register): Boolean = {
    reg match {
      // TODO we will be removing BuiltInReg soon
      // (and we will refactering AddrLocation to be more like loc)
      case reg: BuiltInReg => true
      case _               => false
    }
  }
}

// Offset from some base pointer (usually rbp)
trait MemLoc extends Loc {
  def toString: String
}

// StaticMemLoc exists purely for the purpose of pattern matching
trait StaticMemLoc[T] extends MemLoc {
  val value: T
}
trait Offset extends MemLoc
trait Label extends MemLoc

// Data values can be either scalars (None in the length) or arrays (Some(length: Int))
case class DataLabel(val value: Long, val length: Option[Long])
    extends Label
    with StaticMemLoc[Long] {
  override def toString: String = s"${CodeGenerator.Header.data}$value"
}
// NOTE: all strings are stored in data
case class StringLabel(val value: Long) extends Label with StaticMemLoc[Long] {
  override def toString: String = s"${CodeGenerator.Header.string}$value"
}

case class StaticOffset(val value: Long)
    extends Offset
    with StaticMemLoc[Long] {
  override def toString: String = value.toString
}
case class DynamicOffset(val src: Register) extends Offset {
  override def toString: String = src.toString
}

case class ArrAddrOffset(val base: StaticMemLoc[Long], val index: DynamicOffset)
    extends Offset {
  override def toString: String = s"$base + $index"
}
