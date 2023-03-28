package edu.mit.compilers.semantics
import edu.mit.compilers.parser._

object ReturnType extends Enumeration {
  type Type = Value
  val void = Value("void")
  val int = Value("int")
  val bool = Value("bool")
  def fromString(s: String): Type = s match {
    case "void" => void
    case "int"  => int
    case "bool" => bool
    case _      => throw new Exception("Invalid return type")
  }
  def fromNode(t: ASTTypeNode): Type = fromString(t.token.string)
}

trait Descr {
  // Descriptor for different variables, imports, and methods
  val dtype: ReturnType.Type
  val token: Token
  val name: String = token.string
  val line: Int = token.line
  val col: Int = token.col
}
case class ImportDescr(
    val token: Token
) extends Descr {
  val dtype: ReturnType.Type = ReturnType.int
  override def toString = s"<import> at line $line, col $col: $name"
}
trait VarDescr extends Descr {
  val dtype: ReturnType.Type
}
case class ScalarDescr(
    val token: Token,
    val dtype: ReturnType.Type
) extends VarDescr {
  override def toString = s"<scalar> at line $line, col $col: $dtype $name"
}
case class ArrayDescr(
    val token: Token,
    val dtype: ReturnType.Type,
    val size: Int
) extends VarDescr {
  override def toString =
    s"<array> at line $line, col $col: $dtype $name[$size]"
}
case class ParamDescr(
    val token: Token,
    val dtype: ReturnType.Type
) extends Descr {
  override def toString = s"<param> at line $line, col $col: $dtype $name"
}
case class MethodDescr(
    val token: Token,
    val dtype: ReturnType.Type,
    val params: List[ParamDescr]
) extends Descr {
  override def toString =
    s"<method> at line $line, col $col: $dtype $name(${params.mkString(", ")})"
}

trait Table {
  val node: ASTNode
  val parent: Option[Table]
  def setNode(n: ASTNode): Table
  def addedImport(i: ImportDescr): Table
  def addedScalar(s: ScalarDescr): Table
  def addedArray(a: ArrayDescr): Table
  def addedMethod(m: MethodDescr): Table
  def addedParam(p: ParamDescr): Table
  def maybeLocal(name: String): Option[Descr]
  def hasLocal(name: String): Boolean
  def getLocal(name: String): Descr
  def maybeGlobal(name: String): Option[Descr]
  def hasGlobal(name: String): Boolean
  def getGlobal(name: String): Descr
}

case class Symbols(
    // Symbol table for program

    // TODO: we need a way of tracking whether is necessary to return
    // TODO: we need a way of tracking what type an if/else or return
    // statement should return.
    // TODO: we need a way of searching the table in the proper order.
    val node: ASTNode,
    val parent: Option[Table] = None,
    val imports: Map[String, ImportDescr] = Map(),
    val scalars: Map[String, ScalarDescr] = Map(),
    val arrays: Map[String, ArrayDescr] = Map(),
    val params: Map[String, ParamDescr] = Map(),
    val methods: Map[String, MethodDescr] = Map()
) extends Table {
  def setNode(n: ASTNode): Symbols = copy(node = n)
  def addedImport(i: ImportDescr): Symbols = {
    if (hasLocal(i.name)) {
      node.errors :+= SemanticError(i.token, "Duplicate import")
      this
    } else {
      val newImports = imports + (i.name -> i)
      copy(imports = newImports)
    }
  }
  def addedScalar(s: ScalarDescr): Symbols = {
    if (hasLocal(s.name)) {
      node.errors :+= SemanticError(s.token, "Duplicate scalar")
      this
    } else {
      val newScalars = scalars + (s.name -> s)
      copy(scalars = newScalars)
    }
  }
  def addedArray(a: ArrayDescr): Symbols = {
    if (hasLocal(a.name)) {
      node.errors :+= SemanticError(a.token, "Duplicate array")
      this
    } else {
      val newArrays = arrays + (a.name -> a)
      copy(arrays = newArrays)
    }
  }
  def addedParam(p: ParamDescr): Symbols = {
    if (hasLocal(p.name)) {
      node.errors :+= SemanticError(p.token, "Duplicate parameter")
      this
    } else {
      val newParams = params + (p.name -> p)
      copy(params = newParams)
    }
  }
  def addedMethod(m: MethodDescr): Symbols = {
    if (hasLocal(m.name)) {
      node.errors :+= SemanticError(m.token, "Duplicate method")
      this
    } else {
      val newMethods = methods + (m.name -> m)
      copy(methods = newMethods)
    }
  }
  def maybeLocal(name: String): Option[Descr] = {
    imports.get(name) orElse
      scalars.get(name) orElse
      arrays.get(name) orElse
      params.get(name) orElse
      methods.get(name)
  }
  def hasLocal(name: String): Boolean = {
    maybeLocal(name).isDefined
  }
  def getLocal(name: String): Descr = {
    maybeLocal(name).getOrElse(
      throw new Exception(s"$name is not defined in local scope")
    )
  }
  def maybeGlobal(name: String): Option[Descr] = {
    maybeLocal(name) orElse parent.flatMap(_.maybeGlobal(name))
  }
  def hasGlobal(name: String): Boolean = {
    maybeGlobal(name).isDefined
  }
  def getGlobal(name: String): Descr = {
    maybeGlobal(name).getOrElse(
      throw new Exception(s"$name is not defined in scope tree")
    )
  }
  override def toString() = {
    List(
      imports.map { case (k, v) => s"$k -> $v" }.mkString("\n"),
      scalars.map { case (k, v) => s"$k -> $v" }.mkString("\n"),
      arrays.map { case (k, v) => s"$k -> $v" }.mkString("\n"),
      params.map { case (k, v) => s"$k -> $v" }.mkString("\n"),
      methods.map { case (k, v) => s"$k -> $v" }.mkString("\n")
    ).filter(_.nonEmpty).mkString("\n")
  }
}

case object TableInserter {

  // Visitor function for getting descriptor for each ASTNode
  def describe(n: ASTNode): Descr = n match {
    case id: ASTImportDecl =>
      ImportDescr(id.id.token)
    case sd: ASTScalarDecl =>
      ScalarDescr(
        sd.id.token,
        ReturnType.fromNode(
          sd.parent.orNull.asInstanceOf[ASTFieldDecl].dtype
        )
      )
    case ad: ASTArrayDecl =>
      ArrayDescr(
        ad.id.token,
        ReturnType.fromNode(
          ad.parent.orNull.asInstanceOf[ASTFieldDecl].dtype
        ),
        ad.size.token.string match {
          case s if s.length > 2 && s.startsWith("0x") =>
            Integer.parseInt(s.substring(2), 16)
          case s =>
            Integer.parseInt(s, 10)
        }
      )
    case p: ASTParam =>
      ParamDescr(p.id.token, ReturnType.fromNode(p.dtype))
    case tmd: ASTTypeMethodDecl =>
      MethodDescr(
        tmd.id.token,
        ReturnType.fromNode(tmd.dtype),
        tmd.params.map(describe).map(_.asInstanceOf[ParamDescr])
      )
    case vmd: ASTVoidMethodDecl =>
      MethodDescr(
        vmd.id.token,
        ReturnType.void,
        vmd.params
          .map(describe)
          .map(_.asInstanceOf[ParamDescr])
      )
    case _ => throw new Exception("Invalid node")
  }

  // Visitor function for handling symbol table for each node type
  def visit(n: ASTNode, table: Table): Table = {
    n match {
      case program: ASTProgram => {
        // ASTProgram visits each child to add tables to it
        var newTable = table.setNode(program)
        for (i <- program.imports) {
          newTable = visit(i, newTable)
        }
        for (f <- program.fields) {
          newTable = visit(f, newTable)
        }
        for (m <- program.methods) {
          newTable = visit(m, newTable)
        }
        program.table = newTable
        newTable
      }
      case importDecl: ASTImportDecl => {
        // Just add to our imports table.
        var newTable = table.setNode(importDecl)
        val descr = describe(importDecl).asInstanceOf[ImportDescr]
        newTable = newTable.addedImport(descr)
        newTable = visit(importDecl.id, newTable)
        importDecl.table = newTable
        newTable
      }
      case fieldDecl: ASTFieldDecl => {
        // Just add whatever field is being declared
        // (either scalar or array: both below)
        var newTable = table.setNode(fieldDecl)
        newTable = visit(fieldDecl.dtype, newTable)
        for (v <- fieldDecl.vars) {
          newTable = visit(v, newTable)
        }
        fieldDecl.table = newTable
        newTable
      }
      case scalarDecl: ASTScalarDecl => {
        // Just add the scalar to the table
        var newTable = table.setNode(scalarDecl)
        val descr = describe(scalarDecl).asInstanceOf[ScalarDescr]
        newTable = newTable.addedScalar(descr)
        newTable = visit(scalarDecl.id, newTable)
        scalarDecl.table = newTable
        newTable
      }
      case arrayDecl: ASTArrayDecl => {
        // Just add this array to the table
        var newTable = table.setNode(arrayDecl)
        val descr = describe(arrayDecl).asInstanceOf[ArrayDescr]
        newTable = newTable.addedArray(descr)
        newTable = visit(arrayDecl.id, newTable)
        newTable = visit(arrayDecl.size, newTable)
        arrayDecl.table = newTable
        newTable
      }
      case param: ASTParam => {
        // Just add the param to the current table
        // (correct one, that is, child, must be passed in)
        var newTable = table.setNode(param)
        val descr = describe(param).asInstanceOf[ParamDescr]
        newTable = newTable.addedParam(descr)
        newTable = visit(param.dtype, newTable)
        newTable = visit(param.id, newTable)
        param.table = newTable
        newTable
      }
      case typeMethodDecl: ASTTypeMethodDecl => {
        // Methods with return types
        // 1. Add parameters (to child table)
        // 2. Enter new scope
        var newTable = table.setNode(typeMethodDecl)
        val descr = describe(typeMethodDecl).asInstanceOf[MethodDescr]
        newTable = newTable.addedMethod(descr)
        newTable = visit(typeMethodDecl.dtype, newTable)
        newTable = visit(typeMethodDecl.id, newTable)

        var childTable: Table =
          Symbols(typeMethodDecl, parent = Some(newTable))
        for (p <- typeMethodDecl.params) {
          childTable = visit(p, childTable)
        }
        childTable = visit(typeMethodDecl.block, childTable)

        typeMethodDecl.table = newTable
        newTable
      }
      case voidMethodDecl: ASTVoidMethodDecl => {
        // Void method
        // 1. Add parameters (to child table)
        // 2. Enter new scope
        var newTable = table.setNode(voidMethodDecl)
        val descr = describe(voidMethodDecl).asInstanceOf[MethodDescr]
        newTable = newTable.addedMethod(descr)
        newTable = visit(voidMethodDecl.id, newTable)

        var childTable: Table =
          Symbols(voidMethodDecl, parent = Some(newTable))
        for (p <- voidMethodDecl.params) {
          childTable = visit(p, childTable)
        }
        childTable = visit(voidMethodDecl.block, childTable)

        voidMethodDecl.table = newTable
        newTable
      }
      case block: ASTBlock => {
        // ASTBlock
        // 1. Treat the table as its own thing (parent passes down a new table)
        var newTable = table.setNode(block)
        for (f <- block.fields) {
          newTable = visit(f, newTable)
        }
        for (s <- block.stmts) {
          newTable = visit(s, newTable)
        }
        block.table = newTable
        newTable
      }
      case ifThenStmt: ASTIfThenStmt => {
        // If
        // 1. For condition points to table (without initializing anything,
        //    if it did they'd be in the child table, though it has to only have
        //    access to parent scope fields and above... luckily since the if does
        //    not initialize anything, it doesn't matter)
        // 2. Enter a new scope with a new table
        var newTable = table.setNode(ifThenStmt)
        newTable = visit(ifThenStmt.cond, newTable)

        var childTable: Table = Symbols(ifThenStmt, parent = Some(newTable))
        childTable = visit(ifThenStmt.thenBlock, childTable)

        ifThenStmt.table = newTable
        newTable
      }
      case ifElseStmt: ASTIfElseStmt => {
        // If/Else
        // 1. Same as if statement
        // 2. If/Else both get their own new scope
        var newTable = table.setNode(ifElseStmt)
        newTable = visit(ifElseStmt.cond, newTable)

        var childTable1: Table = Symbols(ifElseStmt, parent = Some(newTable))
        childTable1 = visit(ifElseStmt.thenBlock, childTable1)
        var childTable2: Table = Symbols(ifElseStmt, parent = Some(newTable))
        childTable2 = visit(ifElseStmt.elseBlock, childTable2)

        ifElseStmt.table = newTable
        newTable
      }
      case forStmt: ASTForStmt => {
        // For
        // 1. Same as if statement
        // 2. ASTBlock gets new scope
        var newTable = table.setNode(forStmt)
        newTable = visit(forStmt.id, newTable)
        newTable = visit(forStmt.init, newTable)
        newTable = visit(forStmt.cond, newTable)
        newTable = visit(forStmt.update, newTable)

        var childTable: Table = Symbols(forStmt, parent = Some(newTable))
        childTable = visit(forStmt.block, childTable)

        forStmt.table = newTable
        newTable
      }
      case whileStmt: ASTWhileStmt => {
        // While
        // 1. Same as if statement
        // 2. ASTBlock gets new scope
        var newTable = table.setNode(whileStmt)
        newTable = visit(whileStmt.cond, newTable)

        var childTable: Table = Symbols(whileStmt, parent = Some(newTable))
        childTable = visit(whileStmt.block, childTable)

        whileStmt.table = newTable
        newTable
      }
      case opAssignStmt: ASTOpAssignStmt => {
        var newTable = table.setNode(opAssignStmt)
        newTable = visit(opAssignStmt.location, newTable)
        newTable = visit(opAssignStmt.expr, newTable)
        opAssignStmt.table = newTable
        newTable
      }
      case mutAssignStmt: ASTMutAssignStmt => {
        var newTable = table.setNode(mutAssignStmt)
        newTable = visit(mutAssignStmt.location, newTable)
        mutAssignStmt.table = newTable
        newTable
      }
      case methodName: ASTMethodName => {
        var newTable = table.setNode(methodName)
        newTable = visit(methodName.id, newTable)
        methodName.table = newTable
        newTable
      }
      case methodCallStmt: ASTMethodCallStmt => {
        var newTable = table.setNode(methodCallStmt)
        newTable = visit(methodCallStmt.name, newTable)
        for (a <- methodCallStmt.args) {
          newTable = visit(a, newTable)
        }
        methodCallStmt.table = newTable
        newTable
      }
      case voidReturnStmt: ASTVoidReturnStmt => {
        var newTable = table.setNode(voidReturnStmt)
        voidReturnStmt.table = newTable
        newTable
      }
      case typeReturnStmt: ASTTypeReturnStmt => {
        var newTable = table.setNode(typeReturnStmt)
        newTable = visit(typeReturnStmt.expr, newTable)
        typeReturnStmt.table = newTable
        newTable
      }
      case breakStmt: ASTBreakStmt => {
        var newTable = table.setNode(breakStmt)
        breakStmt.table = newTable
        newTable
      }
      case continueStmt: ASTContinueStmt => {
        var newTable = table.setNode(continueStmt)
        continueStmt.table = newTable
        newTable
      }
      case opForUpdate: ASTOpForUpdate => {
        var newTable = table.setNode(opForUpdate)
        newTable = visit(opForUpdate.location, newTable)
        newTable = visit(opForUpdate.expr, newTable)
        opForUpdate.table = newTable
        newTable
      }
      case mutForUpdate: ASTMutForUpdate => {
        var newTable = table.setNode(mutForUpdate)
        newTable = visit(mutForUpdate.location, newTable)
        mutForUpdate.table = newTable
        newTable
      }
      case scalarLocation: ASTScalarLocation => {
        var newTable = table.setNode(scalarLocation)
        newTable = visit(scalarLocation.id, newTable)
        scalarLocation.table = newTable
        newTable
      }
      case arrayLocation: ASTArrayLocation => {
        var newTable = table.setNode(arrayLocation)
        newTable = visit(arrayLocation.id, newTable)
        newTable = visit(arrayLocation.index, newTable)
        arrayLocation.table = newTable
        newTable
      }
      case methodCallExpr: ASTMethodCallExpr => {
        var newTable = table.setNode(methodCallExpr)
        newTable = visit(methodCallExpr.name, newTable)
        for (a <- methodCallExpr.args) {
          newTable = visit(a, newTable)
        }
        methodCallExpr.table = newTable
        newTable
      }
      case literal: ASTLiteral => {
        var newTable = table.setNode(literal)
        literal.table = newTable
        newTable
      }
      case stringLiteral: ASTStringLiteral => {
        var newTable = table.setNode(stringLiteral)
        stringLiteral.table = newTable
        newTable
      }
      case lenExpr: ASTLenExpr => {
        var newTable = table.setNode(lenExpr)
        newTable = visit(lenExpr.id, newTable)
        lenExpr.table = newTable
        newTable
      }
      case binOpExpr: ASTBinOpExpr => {
        var newTable = table.setNode(binOpExpr)
        newTable = visit(binOpExpr.left, newTable)
        newTable = visit(binOpExpr.right, newTable)
        binOpExpr.table = newTable
        newTable
      }
      case negExpr: ASTNegExpr => {
        var newTable = table.setNode(negExpr)
        newTable = visit(negExpr.expr, newTable)
        negExpr.table = newTable
        newTable
      }
      case notExpr: ASTNotExpr => {
        var newTable = table.setNode(notExpr)
        newTable = visit(notExpr.expr, newTable)
        notExpr.table = newTable
        newTable
      }
      case parenExpr: ASTParenExpr => {
        var newTable = table.setNode(parenExpr)
        newTable = visit(parenExpr.expr, newTable)
        parenExpr.table = newTable
        newTable
      }
      case typeNode: ASTTypeNode => {
        var newTable = table.setNode(typeNode)
        typeNode.table = newTable
        newTable
      }
      case idNode: ASTIdNode => {
        var newTable = table.setNode(idNode)
        idNode.table = newTable
        newTable
      }
      case node: ASTNode =>
        throw new Exception("Unhandled node type: " + node.getClass.getName)
    }
  }
}
