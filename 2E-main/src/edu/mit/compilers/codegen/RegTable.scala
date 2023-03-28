package edu.mit.compilers.codegen

import edu.mit.compilers.parser._

case class RegTable(val parent: Option[RegTable] = None) {
  var nameToRegister: Map[String, Register] = Map()

  // We store a list of the parameters in order from
  // first to last. Recall this table is unique to the function.
  var paramRegs: List[Register] = List()

  def insert(name: String, register: Register): Unit = {
    nameToRegister += (name -> register)
  }
  def delete(name: String): Unit = {
    nameToRegister -= name
  }
  def replace(name: String, register: Register): Unit = {
    delete(name)
    insert(name, register)
  }
  def maybe(name: String): Option[Register] = {
    nameToRegister.get(name) orElse parent.flatMap(_.maybe(name))
  }
  def get(name: String): Register = {
    // Because this runs after semantic checking, this is OK
    maybe(name).getOrElse(
      throw new Exception(s"Failed to find $name in register table")
    )
  }
  def contains(name: String): Boolean = {
    nameToRegister.contains(name) || parent.exists(_.contains(name))
  }

  // Load from a list of parameters (used in CFGManager)
  def loadParams(params: List[ASTParam]): Unit = {
    val paramLocs = RegLocInfo.paramRegLocs
    val firstParams = params.take(paramLocs.size)
    for ((param, loc) <- firstParams.zip(paramLocs)) {
      val name = param.id.token.string
      val reg = RegMaker.tmpBuiltInReg(loc)
      insert(name, reg)
      paramRegs = reg :: paramRegs
    }
    val lastParams = params.drop(paramLocs.size)
    for (param <- lastParams) {
      // Special case, because we need to grow UP the stack
      // from %rbp
      val name = param.id.token.string
      val reg = RegMaker.tmpScalarParamReg(AddrLocation.Stack)
      insert(name, reg)
      paramRegs = reg :: paramRegs
    }
    paramRegs = paramRegs.reverse
  }

  def loadNodes(
      nodes: List[ASTNode],
      location: AddrLocation.Type = AddrLocation.Stack
  ): List[BasicInstr] = {
    var instrs: List[BasicInstr] = List()
    for (n <- nodes) {
      n match {
        case ASTFieldDecl(_, _, vars) => {
          for (v <- vars) {
            // Normal case: grow DOWN the stack from %rbp
            val name = v.id.token.string
            val reg = (v match {
              case ASTArrayDecl(_, id, size) => {
                val arrReg = RegMaker.varArrayBaseReg(
                  id.token.string,
                  size.eval,
                  location
                )
                location match {
                  case AddrLocation.Stack =>
                    instrs = instrs ++ List(DeclInstr(arrReg, size.eval))
                  case AddrLocation.Data =>
                }
                arrReg
              }
              case ASTScalarDecl(_, id) => {
                val scalReg =
                  RegMaker.varScalarReg(location, id.token.string)
                location match {
                  case AddrLocation.Stack =>
                    instrs = instrs ++ List(
                      DeclInstr(scalReg)
                    )
                  case AddrLocation.Data =>
                }
                scalReg
              }
            })
            insert(name, reg)
          }
        }
        case _ =>
      }
    }
    instrs
  }

  override def toString(): String = {
    // n2r has no newlines
    val n2r = nameToRegister.toString
    val sep = "  "
    parent match {
      case Some(p) => p.toString.replace("\n", s"\n$sep") + "\n" + n2r
      case None    => "\n" + n2r
    }
  }
}
