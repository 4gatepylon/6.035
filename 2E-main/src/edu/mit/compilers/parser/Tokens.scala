package edu.mit.compilers.parser

// Traits are like java interfaces. They stipulate that objects or classes that extend
// Token (in this case) must have the following methods/variables
trait Token {
  val string: String
  val name: Rules.Type
  val line: Int
  val col: Int
  def render: String
  override def toString = render
}

// Objects are usually used for singleton classes. I don't totally understand, but you can
// think of them as static objects that will only be insantiated once (according to stackoverflow,
// and that makes sense here because we will only instantiate one of either of these
// since they are used as signals).
object WIP extends Token {
  val string = "WIP"
  val name = Rules.WIP
  val line = -1
  val col = -1
  def render = throw new Exception("token still work in progress")
}

object EOF extends Token {
  val string = "EOF"
  val name = Rules.EOF
  val line = -1
  val col = -1
  def render = "EOF"
}

// Case-classes are basically boilerplate classes of some kind.
case class Keyword(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.Keyword
  def render = s"$line $string"
}

case class IdToken(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.Id
  def render = s"$line IDENTIFIER $string"
}

case class Mark(val string: String, val line: Int, val col: Int) extends Token {
  val name = Rules.Mark
  def render = s"$line $string"
}

case class ArithOp(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.ArithOp
  def render = s"$line $string"
}

case class RelOp(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.RelOp
  def render = s"$line $string"
}

case class EqOp(val string: String, val line: Int, val col: Int) extends Token {
  val name = Rules.EqOp
  def render = s"$line $string"
}

case class CondOp(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.CondOp
  def render = s"$line $string"
}

case class LogOp(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.LogOp
  def render = s"$line $string"
}

case class CompOp(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.CompoundAssignOp
  def render = s"$line $string"
}

case class AssOp(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.AssignOp
  def render = s"$line $string"
}

case class IncOp(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.Increment
  def render = s"$line $string"
}

case class TypeToken(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.Type
  def render = s"$line $string"
}

case class CharToken(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.CharLiteral
  def render = s"$line CHARLITERAL '$string'"
}

case class IntToken(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.IntLiteral
  def render = s"$line INTLITERAL $string"
}

case class BoolToken(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.BoolLiteral
  def render = s"$line BOOLEANLITERAL $string"
}

case class StrToken(val string: String, val line: Int, val col: Int)
    extends Token {
  val name = Rules.StringLiteral
  def render = s"""$line STRINGLITERAL "$string""""
}
