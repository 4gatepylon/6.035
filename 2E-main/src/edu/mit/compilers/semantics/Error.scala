package edu.mit.compilers.semantics

import edu.mit.compilers.parser.{Token}

case class SemanticError(val token: Token, val msg: String) extends Exception {
  // Error class which stores info about token with error message
  val name = token.string
  val line = token.line
  val col = token.col
  override def toString =
    "[ERROR] " + msg + ": " + name + " at line " + line + ", column " + col
}
