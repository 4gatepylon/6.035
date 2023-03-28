package edu.mit.compilers.parser

object Rules extends Enumeration {
  type Type = Value
  val Program = Value("<program>")
  val ImportDecl = Value("<import_decl>")
  val FieldDecl = Value("<field_decl>")
  val VarDecl = Value("<id> | <id> [ <int_literal> ]")
  val MethodDecl = Value("<method_decl>")
  val Param = Value("<type> <id>")
  val Block = Value("<block>")
  val Type = Value("<type>")
  val Statement = Value("<statement>")
  val ForUpdate = Value("<for_update>")
  val AssignExpr = Value("<assign_expr>")
  val AssignOp = Value("<assign_op>")
  val CompoundAssignOp = Value("<compound_assign_op>")
  val Increment = Value("<increment>")
  val MethodCall = Value("<method_call>")
  val MethodName = Value("<method_name>")
  val Location = Value("<location>")
  val Expr = Value("<expr>")
  val ImportArg = Value("<import_arg>")
  val UnOp = Value("<un_op>")
  val BinOp = Value("<bin_op>")
  val ArithOp = Value("<arith_op>")
  val RelOp = Value("<rel_op>")
  val EqOp = Value("<eq_op>")
  val CondOp = Value("<cond_op>")
  val LogOp = Value("<log_op>")
  val CompOp = Value("<comp_op>")
  val Literal = Value("<literal>")
  val CharLiteral = Value("<char_literal>")
  val IntLiteral = Value("<int_literal>")
  val BoolLiteral = Value("<bool_literal>")
  val StringLiteral = Value("<string_literal>")
  val Id = Value("<id>")
  val Mark = Value("<mark>")
  val Keyword = Value("<keyword>")
  val WIP = Value("WIP")
  val EOF = Value("EOF")
}

class Parser(scanner: Scanner) {
  // append EOF token to parse empty input
  val tokens: Seq[Token] = scanner.scan() :+ EOF
  val transitions: Map[Rules.Type, (Int) => (Int, ASTNode)] = Map(
    Rules.Program -> ((i: Int) => {
      // <program> -> <import_decl>* <field_decl>* <method_decl>*
      // <import_decl>*
      val (i0, trees0) = star[ASTImportDecl](i, Rules.ImportDecl)
      // <field_decl>*
      val (i1, trees1) = star[ASTFieldDecl](i0, Rules.FieldDecl)
      // <method_decl>*
      val (i2, trees2) = star[ASTMethodDecl](i1, Rules.MethodDecl)
      val tree = ASTProgram(tokens(i), trees0, trees1, trees2)
      trees0.foreach(_.parent = Some(tree))
      trees1.foreach(_.parent = Some(tree))
      trees2.foreach(_.parent = Some(tree))
      (i2, tree)
    }),
    Rules.ImportDecl -> ((i: Int) => {
      // <import_decl> -> import <id> ';'
      assert(
        tokens(i).string == "import",
        s"Expected 'import' but found ${tokens(i)}"
      )
      val (i0, tree0) = transition[ASTIdNode](i + 1, Rules.Id)
      assert(
        tokens(i0).string == ";",
        s"import_decl: expected ';', found ${tokens(i0)}"
      )
      val tree = ASTImportDecl(tokens(i), tree0)
      tree0.parent = Some(tree)
      (i0 + 1, tree)
    }),
    Rules.FieldDecl -> ((i: Int) => {
      // <field_decl> -> <type> { <id> | <id> '[' <int_literal> ']' }+, ';'
      // <type>
      val (i0, tree0) = transition[ASTTypeNode](i, Rules.Type)
      // { <id> | <id> '[' <int_literal> ']' }+,
      val (i1, trees1) = plusComma[ASTVarDecl](i0, Rules.VarDecl)
      // ;
      assert(
        tokens(i1).string == ";",
        s"field_decl: expected ';', found ${tokens(i1)}"
      )
      val tree = ASTFieldDecl(tokens(i), tree0, trees1)
      tree0.parent = Some(tree)
      trees1.foreach(_.parent = Some(tree))
      (i1 + 1, tree)
    }),
    Rules.VarDecl -> ((i: Int) => {
      // <id> | <id> '[' <int_literal> ']'
      // <id>
      assert(
        tokens(i).isInstanceOf[IdToken],
        s"array: expected <id>, found ${tokens(i)}"
      )
      val (i0, tree0) = transition[ASTIdNode](i, Rules.Id)
      // '[' <int_literal> ']'
      if (i0 < tokens.length && tokens(i0).string == "[") {
        val (i1, tree1) = transition[ASTIntLiteral](i0 + 1, Rules.IntLiteral)
        assert(
          tokens(i1).string == "]",
          s"array: expected ']', found ${tokens(i1)}"
        )
        val tree = ASTArrayDecl(tokens(i), tree0, tree1)
        tree0.parent = Some(tree)
        tree1.parent = Some(tree)
        (i1 + 1, tree)
      } else {
        val tree = ASTScalarDecl(tokens(i), tree0.asInstanceOf[ASTIdNode])
        tree0.parent = Some(tree)
        (i0, tree)
      }
    }),
    Rules.MethodDecl -> ((i: Int) => {
      // <method_decl> -> { <type> | void } <id> '(' [{<type> <id>}+,] ')' <block>
      // <type> | void
      val token = tokens(i)
      assert(
        token.isInstanceOf[TypeToken] || token.string == "void",
        s"method_decl: expected <type> or void, found $token"
      )
      // <id>
      val (i0, tree0) = transition[ASTIdNode](i + 1, Rules.Id)
      // '('
      assert(
        tokens(i0).string == "(",
        s"method_decl: expected '(', found ${tokens(i0)}"
      )
      // [{<type> <id>}+,]
      val (i1, trees1) = bracketPlusComma[ASTParam](i0 + 1, Rules.Param)
      // ')'
      assert(
        tokens(i1).string == ")",
        s"method_decl: expected ')', found ${tokens(i1)}"
      )
      // <block>
      val (i2, tree2) = transition[ASTBlock](i1 + 1, Rules.Block)
      val tree: ASTMethodDecl = if (token.isInstanceOf[TypeToken]) {
        val typeToken = token.asInstanceOf[TypeToken]
        ASTTypeMethodDecl(
          typeToken,
          ASTTypeNode(typeToken),
          tree0,
          trees1,
          tree2
        )
      } else {
        val voidToken = token.asInstanceOf[Keyword]
        ASTVoidMethodDecl(voidToken, tree0, trees1, tree2)
      }
      tree0.parent = Some(tree)
      trees1.foreach(_.parent = Some(tree))
      tree2.parent = Some(tree)
      (i2, tree)
    }),
    Rules.Param -> ((i: Int) => {
      // <param> -> <type> <id>
      // <type>
      val (i0, tree0) = transition[ASTTypeNode](i, Rules.Type)
      // <id>
      val (i1, tree1) = transition[ASTIdNode](i0, Rules.Id)
      val tree = ASTParam(tokens(i), tree0, tree1)
      tree0.parent = Some(tree)
      tree1.parent = Some(tree)
      (i1, tree)
    }),
    Rules.Block -> ((i: Int) => {
      // <block> -> '{' <field_decl>* <statement>* '}'
      // '{'
      assert(
        tokens(i).string == "{",
        s"block: expected '{', found ${tokens(i)}"
      )
      // <field_decl>*
      val (i0, trees0) = star[ASTFieldDecl](i + 1, Rules.FieldDecl)
      // <statement>*
      val (i1, trees1) = star[ASTStmt](i0, Rules.Statement)
      // '}'
      assert(
        tokens(i1).string == "}",
        s"block: expected '}', found ${tokens(i1)}"
      )
      val braceToken = tokens(i).asInstanceOf[Mark]
      val tree = ASTBlock(braceToken, trees0, trees1)
      trees0.foreach(_.parent = Some(tree))
      trees1.foreach(_.parent = Some(tree))
      (i1 + 1, tree)
    }),
    Rules.Statement -> ((i: Int) => {
      // <statement>
      //  -> <location> <assign_expr> ';'
      //   | <method_call> ';'
      //   | if '(' <expr> ')' <block> [else <block>]
      //   | for '(' <id> = <expr> ';' <expr> ';' <for_update> ')' <block>
      //   | while '(' <expr> ')' <block>
      //   | return [<expr>] ';'
      //   | break ';'
      //   | continue ';'
      val token = tokens(i)
      if (token.isInstanceOf[IdToken]) {
        // <location> <assign_expr> ';' | <method_call> ';'
        if (tokens(i + 1).string == "(") {
          // <method_call> ';'
          // <method_call> -> <method_name> '(' [<import_arg>+,] ')'
          // <method_name>
          val (i0, tree0) = transition[ASTMethodName](i, Rules.MethodName)
          // '('
          assert(
            tokens(i0).string == "(",
            s"method_call: expected '(', found ${tokens(i0)}"
          )
          // [<import_arg>+,]
          val (i1, trees1) =
            bracketPlusComma[ASTImportArg](i0 + 1, Rules.ImportArg)
          // ')'
          assert(
            tokens(i1).string == ")",
            s"method_call: expected ')', found ${tokens(i1)}"
          )
          // ';'
          assert(
            tokens(i1 + 1).string == ";",
            s"statement: expected ';', found ${tokens(i1 + 1)}"
          )
          val tree = ASTMethodCallStmt(token, tree0, trees1)
          tree0.parent = Some(tree)
          trees1.foreach(_.parent = Some(tree))
          (i1 + 2, tree)
        } else {
          // <location> <assign_expr> ';'
          val (i0, tree0) = transition[ASTLocation](i, Rules.Location)
          val token0 = tokens(i0)
          if (tokens(i0 + 1).string == ";") {
            val tree = token0.string match {
              case "++" => ASTIncrStmt(token0, tree0)
              case "--" => ASTDecrStmt(token0, tree0)
            }
            tree0.parent = Some(tree)
            (i0 + 2, tree)
          } else {
            val (i1, tree1) = transition[ASTExpr](i0 + 1, Rules.Expr)
            assert(
              tokens(i1).string == ";",
              s"statement: expected ';', found ${tokens(i1)}"
            )
            val tree = token0.string match {
              case "="  => ASTEqAssignStmt(token0, tree0, tree1)
              case "+=" => ASTAddAssignStmt(token0, tree0, tree1)
              case "-=" => ASTSubAssignStmt(token0, tree0, tree1)
            }
            tree0.parent = Some(tree)
            tree1.parent = Some(tree)
            (i1 + 1, tree)
          }
        }
      } else if (token.string == "if") {
        // if '(' <expr> ')' <block> else <block>
        val ifToken = token.asInstanceOf[Keyword]
        // '('
        assert(
          tokens(i + 1).string == "(",
          s"statement: expected '(', found ${tokens(i + 1)}"
        )
        // <expr>
        val (i0, tree0) = transition[ASTExpr](i + 2, Rules.Expr)
        // ')'
        assert(
          tokens(i0).string == ")",
          s"statement: expected ')', found ${tokens(i0)}"
        )
        // <block>
        val (i1, tree1) = transition[ASTBlock](i0 + 1, Rules.Block)
        // [else <block>]
        if (tokens(i1).string == "else") {
          val (i2, tree2) = transition[ASTBlock](i1 + 1, Rules.Block)
          val tree = ASTIfElseStmt(ifToken, tree0, tree1, tree2)
          tree0.parent = Some(tree)
          tree1.parent = Some(tree)
          tree2.parent = Some(tree)
          (i2, tree)
        } else {
          val tree = ASTIfThenStmt(ifToken, tree0, tree1)
          tree0.parent = Some(tree)
          tree1.parent = Some(tree)
          (i1, tree)
        }
      } else if (token.string == "for") {
        // for '(' <id> = <expr> ';' <expr> ';' <for_update> ')' <block>
        val forToken = token.asInstanceOf[Keyword]
        // '('
        assert(
          tokens(i + 1).string == "(",
          s"statement: expected '(', found ${tokens(i + 1)}"
        )
        // <id>
        val (i0, tree0) = transition[ASTIdNode](i + 2, Rules.Id)
        // =
        assert(
          tokens(i0).string == "=",
          s"statement: expected '=', found ${tokens(i0)}"
        )
        // <expr>
        val (i1, tree1) = transition[ASTExpr](i0 + 1, Rules.Expr)
        // ';'
        assert(
          tokens(i1).string == ";",
          s"statement: expected ';', found ${tokens(i1)}"
        )
        // <expr>
        val (i2, tree2) = transition[ASTExpr](i1 + 1, Rules.Expr)
        // ';'
        assert(
          tokens(i2).string == ";",
          s"statement: expected ';', found ${tokens(i2)}"
        )
        // <for_update>
        val (i3, tree3) = transition[ASTForUpdate](i2 + 1, Rules.ForUpdate)
        // ')'
        assert(
          tokens(i3).string == ")",
          s"statement: expected ')', found ${tokens(i3)}"
        )
        // <block>
        val (i4, tree4) = transition[ASTBlock](i3 + 1, Rules.Block)
        val tree = ASTForStmt(forToken, tree0, tree1, tree2, tree3, tree4)
        tree0.parent = Some(tree)
        tree1.parent = Some(tree)
        tree2.parent = Some(tree)
        tree3.parent = Some(tree)
        tree4.parent = Some(tree)
        (i4, tree)
      } else if (token.string == "while") {
        // while '(' <expr> ')' <block>
        val whileToken = token.asInstanceOf[Keyword]
        // '('
        assert(
          tokens(i + 1).string == "(",
          s"statement: expected '(', found ${tokens(i + 1)}"
        )
        // <expr>
        val (i0, tree0) = transition[ASTExpr](i + 2, Rules.Expr)
        // ')'
        assert(
          tokens(i0).string == ")",
          s"statement: expected ')', found ${tokens(i0)}"
        )
        // <block>
        val (i1, tree1) = transition[ASTBlock](i0 + 1, Rules.Block)
        val tree = ASTWhileStmt(whileToken, tree0, tree1)
        tree0.parent = Some(tree)
        tree1.parent = Some(tree)
        (i1, tree)
      } else if (token.string == "return") {
        // return [<expr>] ';'
        val returnToken = token.asInstanceOf[Keyword]
        if (tokens(i + 1).string == ";") {
          val tree = ASTVoidReturnStmt(returnToken)
          (i + 2, tree)
        } else {
          val (i0, tree0) = transition[ASTExpr](i + 1, Rules.Expr)
          assert(
            tokens(i0).string == ";",
            s"statement: expected ';', found ${tokens(i0)}"
          )
          val tree = ASTTypeReturnStmt(returnToken, tree0)
          tree0.parent = Some(tree)
          (i0 + 1, tree)
        }
      } else if (token.string == "break") {
        // break ';'
        val breakToken = token.asInstanceOf[Keyword]
        // ';'
        assert(
          tokens(i + 1).string == ";",
          s"statement: expected ';', found ${tokens(i + 1)}"
        )
        val tree = ASTBreakStmt(breakToken)
        (i + 2, tree)
      } else if (token.string == "continue") {
        // continue ';'
        val continueToken = token.asInstanceOf[Keyword]
        // ';'
        assert(
          tokens(i + 1).string == ";",
          s"statement: expected ';', found ${tokens(i + 1)}"
        )
        val tree = ASTContinueStmt(continueToken)
        (i + 2, tree)
      } else {
        throw new Exception(s"statement: invalid starting token $token")
      }
    }),
    Rules.ForUpdate -> ((i: Int) => {
      // <location> { <compound_assign_op> <expr> | <increment> }
      val idToken: IdToken = tokens(i).asInstanceOf[IdToken]
      val (i0, tree0) = transition[ASTLocation](i, Rules.Location)
      val token0 = tokens(i0)
      if (token0.isInstanceOf[IncOp]) {
        val tree = token0.string match {
          case "++" => ASTIncrForUpdate(token0, tree0)
          case "--" => ASTDecrForUpdate(token0, tree0)
        }
        tree0.parent = Some(tree)
        (i0 + 1, tree)
      } else {
        val (i1, tree1) = transition[ASTExpr](i0 + 1, Rules.Expr)
        val tree = token0.string match {
          case "+=" => ASTAddForUpdate(token0, tree0, tree1)
          case "-=" => ASTSubForUpdate(token0, tree0, tree1)
        }
        tree0.parent = Some(tree)
        tree1.parent = Some(tree)
        (i1, tree)
      }
    }),
    Rules.MethodName -> ((i: Int) => {
      // <method_name> -> <id>
      val (i0, tree0) = transition[ASTIdNode](i, Rules.Id)
      val tree = ASTMethodName(tokens(i), tree0)
      tree0.parent = Some(tree)
      (i0, tree)
    }),
    Rules.Location -> ((i: Int) => {
      // <location> -> <id> | <id> '[' <expr> ']'
      // <id>
      val idToken: IdToken = tokens(i).asInstanceOf[IdToken]
      val (i0, tree0) = transition[ASTIdNode](i, Rules.Id)
      if (i0 < tokens.length && tokens(i0).string == "[") {
        // <expr>
        val (i1, tree1) = transition[ASTExpr](i0 + 1, Rules.Expr)
        // ']'
        assert(
          tokens(i1).string == "]",
          s"location: expected ']', found ${tokens(i1)}"
        )
        val tree = ASTArrayLocation(idToken, tree0, tree1)
        tree0.parent = Some(tree)
        tree1.parent = Some(tree)
        (i1 + 1, tree)
      } else {
        val tree = ASTScalarLocation(idToken, tree0)
        tree0.parent = Some(tree)
        (i0, tree)
      }
    }),
    Rules.Expr -> ((i: Int) => {
      // <expr>
      //  -> <location>
      //   | <method_call>
      //   | <literal>
      //   | len '(' <id> ')'
      //   | - <expr>
      //   | ! <expr>
      //   | '(' <expr> ')'
      // <expr> -> <expr> [<bin_op> <expr>]
      val token = tokens(i)
      val (j, left: ASTExpr) =
        if (token.isInstanceOf[IntToken]) {
          // <int_literal>
          transition[ASTIntLiteral](i, Rules.IntLiteral)
        } else if (token.isInstanceOf[CharToken]) {
          // <char_literal>
          transition[ASTCharLiteral](i, Rules.CharLiteral)
        } else if (token.isInstanceOf[BoolToken]) {
          // <bool_literal>
          transition[ASTBoolLiteral](i, Rules.BoolLiteral)
        } else if (token.isInstanceOf[IdToken]) {
          // <location> | <method_call>
          if (i + 1 < tokens.length && tokens(i + 1).string == "(") {
            // <method_call> -> <method_name> '(' [<import_arg>+,] ')'
            // <method_name>
            val (i0, tree0) = transition[ASTMethodName](i, Rules.MethodName)
            // '('
            assert(
              tokens(i0).string == "(",
              s"method_call: expected '(', found ${tokens(i0)}"
            )
            // [<import_arg>+,]
            val (i1, trees1) =
              bracketPlusComma[ASTImportArg](i0 + 1, Rules.ImportArg)
            // ')'
            assert(
              tokens(i1).string == ")",
              s"method_call: expected ')', found ${tokens(i1)}"
            )
            val tree = ASTMethodCallExpr(token, tree0, trees1)
            tree0.parent = Some(tree)
            trees1.foreach(_.parent = Some(tree))
            (i1 + 1, tree)
          } else {
            // <location>
            transition[ASTLocation](i, Rules.Location)
          }
        } else if (token.string == "len") {
          // len '(' <id> ')'
          // len
          val lenToken = token.asInstanceOf[Keyword]
          // '('
          assert(
            tokens(i + 1).string == "(",
            s"expr: expected '(', found ${tokens(i + 1)}"
          )
          // <id>
          val (i0, tree0) = transition[ASTIdNode](i + 2, Rules.Id)
          // ')'
          assert(
            tokens(i0).string == ")",
            s"expr: expected ')', found ${tokens(i0)}"
          )
          val tree = ASTLenExpr(lenToken, tree0)
          tree0.parent = Some(tree)
          (i0 + 1, tree)
        } else if (token.string == "-") {
          // - <expr>
          // -
          val negToken = token.asInstanceOf[ArithOp]
          // <expr>
          val (i0, tree0) = transition[ASTExpr](i + 1, Rules.Expr)
          val tree = ASTNegExpr(negToken, tree0)
          tree0.parent = Some(tree)
          (i0, tree)
        } else if (token.string == "!") {
          // ! <expr>
          // !
          val notToken = token.asInstanceOf[LogOp]
          // <expr>
          val (i0, tree0) = transition[ASTExpr](i + 1, Rules.Expr)
          val tree = ASTNotExpr(notToken, tree0)
          tree0.parent = Some(tree)
          (i0, tree)
        } else if (token.string == "(") {
          // '(' <expr> ')'
          val parenToken = token.asInstanceOf[Mark]
          // <expr>
          val (i0, tree0) = transition[ASTExpr](i + 1, Rules.Expr)
          // ')'
          assert(
            tokens(i0).string == ")",
            s"expr: expected ')', found ${tokens(i0)}"
          )
          val tree = ASTParenExpr(parenToken, tree0)
          tree0.parent = Some(tree)
          (i0 + 1, tree)
        } else {
          throw new Exception(s"expr: invalid starting token $token")
        }
      try {
        // [<bin_op> <expr>]
        val binOp = tokens(j)
        val (j0, right) = transition[ASTExpr](j + 1, Rules.Expr)
        binOp match {
          case arithOp: ArithOp => {
            val tree = arithOp.string match {
              case "+" => ASTAddExpr(arithOp, left, right)
              case "-" => ASTSubExpr(arithOp, left, right)
              case "*" => ASTMulExpr(arithOp, left, right)
              case "/" => ASTDivExpr(arithOp, left, right)
              case "%" => ASTModExpr(arithOp, left, right)
            }
            left.parent = Some(tree)
            right.parent = Some(tree)
            (j0, tree)
          }
          case relOp: RelOp => {
            val tree = relOp.string match {
              case "<"  => ASTLtExpr(relOp, left, right)
              case "<=" => ASTLeExpr(relOp, left, right)
              case ">"  => ASTGtExpr(relOp, left, right)
              case ">=" => ASTGeExpr(relOp, left, right)
            }
            left.parent = Some(tree)
            right.parent = Some(tree)
            (j0, tree)
          }
          case eqOp: EqOp => {
            val tree = eqOp.string match {
              case "==" => ASTEqExpr(eqOp, left, right)
              case "!=" => ASTNeExpr(eqOp, left, right)
            }
            left.parent = Some(tree)
            right.parent = Some(tree)
            (j0, tree)
          }
          case condOp: CondOp => {
            val tree = condOp.string match {
              case "&&" => ASTAndExpr(condOp, left, right)
              case "||" => ASTOrExpr(condOp, left, right)
            }
            left.parent = Some(tree)
            right.parent = Some(tree)
            (j0, tree)
          }
        }
      } catch {
        case e: Throwable => (j, left)
      }
    }),
    Rules.ImportArg -> ((i: Int) => {
      // <import_arg> -> <expr> | <string_literal>
      val token = tokens(i)
      if (token.isInstanceOf[StrToken]) {
        // <string_literal>
        transition[ASTStringLiteral](i, Rules.StringLiteral)
      } else {
        // <expr>
        transition[ASTExpr](i, Rules.Expr)
      }
    }),
    Rules.StringLiteral -> ((i: Int) => {
      // <string_literal>
      (i + 1, ASTStringLiteral(tokens(i).asInstanceOf[StrToken]))
    }),
    Rules.IntLiteral -> ((i: Int) => {
      // <int_literal>
      (i + 1, ASTIntLiteral(tokens(i).asInstanceOf[IntToken]))
    }),
    Rules.CharLiteral -> ((i: Int) => {
      // <char_literal>
      (i + 1, ASTCharLiteral(tokens(i).asInstanceOf[CharToken]))
    }),
    Rules.BoolLiteral -> ((i: Int) => {
      // <bool_literal>
      (i + 1, ASTBoolLiteral(tokens(i).asInstanceOf[BoolToken]))
    }),
    Rules.Id -> ((i: Int) => {
      // <id>
      (i + 1, ASTIdNode(tokens(i).asInstanceOf[IdToken]))
    }),
    Rules.Type -> ((i: Int) => {
      // <type>
      (i + 1, ASTTypeNode(tokens(i).asInstanceOf[TypeToken]))
    })
  )

  def star[T](i: Int, rule: Rules.Type): (Int, List[T]) = {
    // <rule>*
    // <rule> cannot generate epsilon
    var idx = i
    var children = List[T]()
    while (true) {
      try {
        val (ruleIndex, ruleChild) = transition[T](idx, rule)
        assert(
          ruleIndex > idx,
          "sanity check that successful parse consumed something"
        )
        idx = ruleIndex
        children :+= ruleChild
      } catch {
        case e: Throwable => {
          if (scanner.debug) {
            println(s"star: idx = $idx, rule = $rule, error = ${e.getMessage}")
          }
          return (idx, children)
        }
      }
    }
    throw new Exception("star: should not reach here")
  }

  def plusComma[T](i: Int, rule: Rules.Type): (Int, List[T]) = {
    // <rule>+,
    // <rule> cannot generate epsilon
    var idx = i
    var children = List[T]()
    // 1. <rule>
    val (ruleIndex, ruleChild) = transition[T](idx, rule)
    idx = ruleIndex
    children :+= ruleChild
    // 2. (',' <rule>)*
    while (true) {
      if (idx < tokens.length && tokens(idx).string == ",") {
        // ','
        idx += 1
        // <rule>
        val (ruleIndex, ruleChild) = transition[T](idx, rule)
        idx = ruleIndex
        children :+= ruleChild
      } else {
        return (idx, children)
      }
    }
    throw new Exception("commas: should not reach here")
  }

  def bracketPlusComma[T](i: Int, rule: Rules.Type): (Int, List[T]) = {
    // [<rule>+,]
    // <rule> cannot generate epsilon
    try {
      plusComma[T](i, rule)
    } catch {
      case e: Throwable => (i, List.empty)
    }
  }

  def transition[T](i: Int, rule: Rules.Type): (Int, T) = {
    val (idx, tree) = transitions(rule)(i)
    if (scanner.debug) {
      println(s"transition: start $i, rule $rule, end $idx, tree $tree")
    }
    (idx, tree.asInstanceOf[T])
  }

  /** Parse the tokens obtained from `scanner` */
  def parse(): ASTNode = {
    if (scanner.debug) {
      println("tokens:")
      tokens.foreach(println)
    }
    val (idx, tree) = transition[ASTNode](0, Rules.Program)
    if (scanner.debug) {
      println("leftover tokens: " + tokens.drop(idx).mkString(", "))
    }
    assert(
      idx == tokens.length - 1 && tokens(idx) == EOF,
      "sanity check that all tokens were consumed"
    )
    return tree
  }

  /** Whether an error occurred during parsing */
  def hasError: Boolean = { false }
}
