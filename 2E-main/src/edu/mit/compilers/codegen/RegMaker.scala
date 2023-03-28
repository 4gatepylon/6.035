package edu.mit.compilers.codegen

import edu.mit.compilers.parser._

object RegMaker {
  var currentId: Int = 0

  // Mantains the invariant that the of x
  // will be unique.
  def withUniqueId[T](x: T): T = {
    currentId += 1
    x
  }

  def tmpBuiltInReg(loc: RegLoc): RegLocRegister =
    withUniqueId[RegLocRegister](RegLocRegister(loc))

  // NOTE: RegMaker should be stateless
  def tmpScalarStackReg(): ScalarAddr =
    withUniqueId[ScalarAddr](ScalarAddr(AddrLocation.Stack, currentId))

  def varScalarReg(location: AddrLocation.Type, varName: String): ScalarAddr =
    withUniqueId[ScalarAddr](ScalarAddr(location, currentId, Some(varName)))

  def tmpScalarParamReg(location: AddrLocation.Type): ScalarAddr =
    withUniqueId[ScalarAddr](ScalarAddr(location, currentId))

  // All string registers go on data
  // NOTE that strRegs are unique (i.e. different strings get
  // different regs), but they may have the same space allocated
  // (i.e. with the same label)
  def strReg(string: String): StrAddr =
    withUniqueId[StrAddr](StrAddr(string, AddrLocation.Data, currentId))

  def varArrayBaseReg(
      varName: String,
      length: Long,
      location: AddrLocation.Type
  ): ArrBaseAddr =
    withUniqueId[ArrBaseAddr](ArrBaseAddr(location, currentId, length, varName))

  def tmpArrayIndexReg(
      base: ArrBaseAddr,
      index: Register,
      location: AddrLocation.Type
  ): ArrElemAddr =
    withUniqueId[ArrElemAddr](ArrElemAddr(base, index, location, currentId))

  // Literals are in the instruction stream = no allocation
  def litReg(n: ASTLiteral): Register = {
    val str = n.token.string
    n match {
      case int: ASTIntLiteral   => IntConstReg(int.eval)
      case bool: ASTBoolLiteral => BoolConstReg(str.toBoolean)
      case char: ASTCharLiteral => CharConstReg(str.charAt(0))
      case _                    => throw new Exception(s"Unexpected literal $n")
    }
  }
}
