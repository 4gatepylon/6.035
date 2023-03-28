package edu.mit.compilers.codegen

// Registers are things that we can write to and read from using
// BasicInstrs. They are called this like in LLVM IR.
// https://llvm.org/
// https://www.youtube.com/watch?v=wt7a5BOztuM

// TODO should be renamed to something like "variable"
trait Register {
  def name: String
  override def hashCode: Int = name.hashCode
  def toString: String
  def argList: List[Register] = List(this)
}

// These exist purely for pattern matching sake
trait BuiltInReg extends Register {}
trait ArrayReg extends Register {}
trait ParamReg extends Register {}

// Used to port over to RegLoc from Registers (enables backwards compatibility)
case class RegLocRegister(val loc: RegLoc) extends BuiltInReg with ParamReg {
  override def name = loc.toString
  override def toString = name
}

case object StrFormat extends BuiltInReg with ArrayReg {
  override def name = "strFormat"
  override def toString = "strFormat(%rip)"
}

case object IntFormat extends BuiltInReg with ArrayReg {
  override def name = "intFormat"
  override def toString = "intFormat(%rip)"
}

// Constants are things like $3, $4, etcetera that you will find inlined in the assembly
trait ConstReg extends Register {
  val value: Any
  override def name = value.toString
  override def toString: String = name
}
case class BoolConstReg(val value: Boolean) extends ConstReg
case class IntConstReg(val value: Long) extends ConstReg
case class CharConstReg(val value: Char) extends ConstReg

// TODO this is gonna have to be replaced with global/scalar (refactor for clarity)
object AddrLocation extends Enumeration {
  type Type = Value
  val Stack = Value("stack")
  val Data = Value("data")
}

trait Addr extends Register {
  // Id is used in part to allow unique hashing (look at register and `name`)
  val id: Int
  val location: AddrLocation.Type

  def name: String = s"%$id"
}

case class ScalarAddr(
    val location: AddrLocation.Type,
    val id: Int,
    val varNameOpt: Option[String] = None
) extends Addr {
  override def toString = {
    val offset = RegAllocator.regToLoc.get(this)
    s"${varNameOpt.getOrElse(s"t$id")}: $location[${offset.getOrElse("tbd")}]"
  }
}
case class ArrBaseAddr(
    val location: AddrLocation.Type,
    val id: Int,
    // Array bases are never temps
    val length: Long,
    val varName: String
) extends Addr
    with ArrayReg {
  override def toString = {
    val offset = RegAllocator.regToLoc.get(this)
    s"$varName: $location[${offset.getOrElse("tbd")}]Arr"
  }
}

case class ArrElemAddr(
    val base: ArrBaseAddr,
    val index: Register,
    val location: AddrLocation.Type,
    val id: Int
) extends Addr {
  override def equals(that: Any): Boolean =
    that.isInstanceOf[ArrElemAddr] && (this.hashCode == that.hashCode)
  override def name: String = s"${base.id}[${index.name}]"
  override def toString = s"$base[$index]"
  override def argList: List[Register] =
    List(this) ++ base.argList ++ index.argList
}
// Basically the same as ArrBaseAddr EXCEPT codegen will fill
// all elements of the array with the appropriate chars when referenced
case class StrAddr(
    val value: String,
    val location: AddrLocation.Type,
    val id: Int
) extends Addr
    with ArrayReg {
  override def toString = {
    val offset = RegAllocator.regToLoc.get(this)
    s"Str${offset.getOrElse("tbd")}"
  }
}
