package compile

import edu.mit.compilers.parser.{Parser, Scanner, ASTNode}
import edu.mit.compilers.semantics.{
  Descr,
  MethodDescr,
  ReturnType,
  ParamDescr,
  Symbols,
  TableInserter,
  Collector,
  Checker,
  Pemdas
}
import edu.mit.compilers.codegen.{CFGManager, CodeGenerator}
import edu.mit.compilers.optimization.{
  CSE,
  DCE,
  CP,
  AlgSimp,
  CFGOptimizer,
  Optimization,
  RegAlloc
}
import util.CLI
import scala.util.parsing.input.Reader
import scala.util.parsing.input.StreamReader
import scala.collection.immutable.PagedSeq
import java.io._
import scala.io.Source
import scala.collection.mutable.{StringBuilder, ListBuffer}
import scala.Console

object Compiler {
  var outFile = if (CLI.outfile == null) {
    Console.out
  } else {
    new java.io.PrintStream(new java.io.FileOutputStream(CLI.outfile))
  }

  def main(args: Array[String]): Unit = {
    val opts: List[Optimization] = List(
      CSE,
      DCE,
      CP,
      AlgSimp,
      RegAlloc
    )
    CLI.parse(args, opts.map(_.toString).toArray[String]);

    val output: Option[String] =
      if (CLI.target == CLI.Action.SCAN) {
        scan(CLI.infile)
      } else if (CLI.target == CLI.Action.PARSE) {
        parse(CLI.infile)
      } else if (CLI.target == CLI.Action.INTER) {
        inter(CLI.infile)
      } else if (CLI.target == CLI.Action.ASSEMBLY) {
        // NOTE outfile should never be null since we
        // check at the end of parse(...) for the CLI object
        // (and infile must be provided).
        for (i <- 0 until CLI.opts.length) {
          if (CLI.opts(i) == true) {
            CFGOptimizer.add(opts(i))
          }
        }
        assembly(CLI.infile, CLI.outfile)
      } else {
        None
      }

    if (output.isDefined) {
      println(output.get)
      System.exit(0)
    } else {
      System.exit(1)
    }
  }

  def scan(fileName: String): Option[String] = {
    try {
      val inputStream: FileInputStream = new java.io.FileInputStream(fileName)
      val scanner = new Scanner(
        scala.io.Source.fromInputStream(inputStream).mkString,
        CLI.debug
      )
      Option(scanner.scan().map(_.render).mkString("\n"))
    } catch {
      case ex: Exception => {
        Console.err.println(ex)
        None
      }
    }
  }

  /** Parse the file specified by the fileName. Eventually, this method may
    * return a type specific to your compiler.
    */
  def parseRoot(fileName: String): ASTNode = {
    val inputStream: java.io.FileInputStream =
      try {
        new java.io.FileInputStream(fileName)
      } catch {
        case _: FileNotFoundException =>
          Console.err.println("File " + fileName + " does not exist");
          null
      }
    val scanner = new Scanner(
      scala.io.Source.fromInputStream(inputStream).mkString,
      CLI.debug
    )
    val parser = new Parser(scanner)
    val root = parser.parse()
    if (parser.hasError) {
      throw new Exception("Error during parsing.")
    }
    root
  }

  // The parse method simply wraps the parseAndReturn
  // which enables us to use parseAndReturn for more modular
  // coding in semantic checking and code generation
  def parse(fileName: String): Option[String] = {
    try {
      val root = parseRoot(fileName)
      Some(root.toString)
    } catch {
      case ex: Exception => {
        Console.err.println(ex)
        None
      }
    }
  }

  def semanticCheckRoot(root: ASTNode): ASTNode = {
    // High level idea
    // 0. Pemdas
    // 1. Generate symbol tables
    // 2. Do a bunch of checkers documented by rules in the Checkers file
    // 3. Concatenate errors upwards (created by checkers)
    Pemdas.order(root)
    TableInserter.visit(root, Symbols(root))

    // insert errors into nodes
    Checker.visit(root)
    val errors = Collector.visit(root)
    if (errors.nonEmpty) {
      throw new Exception(
        "Semantic errors found:\n" + errors.mkString("\n")
      )
    }
    root
  }

  def inter(fileName: String): Option[String] = {
    try {
      val parsedRoot = parseRoot(fileName)
      val checkedRoot = semanticCheckRoot(parsedRoot)
      Some(checkedRoot.toString)
    } catch {
      case ex: Exception => {
        Console.err.println(ex)
        None
      }
    }
  }

  // Return the assembly code for the given CFGManager's program
  def generateCode(root: ASTNode): String = {
    val manager = new CFGManager(root)
    manager.mkCFGs()

    // TODO (remove this debug)
    // println("********************* CFG Manager *********************")
    // println(manager.toString)
    // println("*******************************************************")

    manager.optCFGs()
    manager.allocCFGs()

    // TODO (remove this debug)
    // println("********************* CFG Manager *********************")
    // println(manager.toString)
    // println("*******************************************************")

    // NOTE: this will NOT run if you do not run allocCFGs() first
    CodeGenerator.emit(manager)
  }

  // Do unoptimized (for now) codegen
  def assembly(
      inputFileName: String,
      outputFileName: String
  ): Option[String] = {
    try {
      val parsedRoot = parseRoot(inputFileName)
      val checkedRoot = semanticCheckRoot(parsedRoot)
      val code = generateCode(checkedRoot)
      // https://www.educba.com/scala-write-to-file/
      val pw = new java.io.PrintWriter(outputFileName)
      pw.write(code)
      pw.close()
      // NOTE that for whatever reason they do something in a temp directory
      // that I couldn't figure out. We get weird illegal character errors
      // even for empty files.
      Option(code)
    } catch {
      case ex: Exception => {
        Console.err.println(ex)
        None
      }
    }
  }
}
