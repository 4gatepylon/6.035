package edu.mit.compilers.parser

object ASCII {
  final val First = 32
  final val Last = 126
}

object Patterns {
  final val Digit = "[0-9]"
  final val HexDigit = "[0-9a-fA-F]"
  final val Alpha = "[a-zA-Z_]"
  final val AlphaNum = "[a-zA-Z_0-9]"
  final val Whitespace = "[ \n\t]"
}

object Sets {
  final val Escaped = Set("\"", "\'", "\\", "t", "n")
  final val Marks = Set("(", ")", "[", "]", "{", "}", ",", ";")
  final val ArithOps = Set("+", "-", "*", "/", "%")
  final val RelOps = Set("<", ">", "<=", ">=")
  final val EqOps = Set("==", "!=")
  final val CondOps = Set("&&", "||")
  final val LogOps = Set("!")
  final val CompOps = Set("+=", "-=")
  final val AssOps = Set("=")
  final val IncOps = Set("++", "--")
  final val BinOps = ArithOps ++ RelOps ++ EqOps ++ CondOps
  final val UnOps = Set("-") ++ LogOps
  final val Ops = BinOps ++ UnOps ++ IncOps ++ CompOps ++ AssOps
  final val Bools = Set("true", "false")
  final val Types = Set("bool", "int")
  final val Keywords = Set(
    "break",
    "import",
    "continue",
    "else",
    "for",
    "while",
    "if",
    "return",
    "len",
    "void"
  )
}

object State extends Enumeration {
  type Type = Value
  val Space = Value("Whitespace")
  val Single = Value("Single-line comment")
  val Multi = Value("Multiline comment")
  val Str = Value("String")
  val Char = Value("Character")
  val Decimal = Value("Decimal")
  val Hex = Value("Hexadecimal")
  val Id = Value("Identifier")
  val Mark = Value("Mark")
  val ArithOp = Value("Arithmetic Operator")
  val RelOp = Value("Relational Operator")
  val EqOp = Value("Equality Operator")
  val CondOp = Value("Conditional Operator")
  val LogOp = Value("Logical Operator")
  val CompOp = Value("Compound Assignment Operator")
  val AssOp = Value("Assignment Operator")
  val IncOp = Value("Increment Operator")
}

class Scanner(text: String, val debug: Boolean = false) {
  // immutable position
  case class Position(idx: Int, row: Int, col: Int) {
    override def toString = s"line $row:$col:"
    // update indices after eating `numChars` characters
    def eat(numChars: Int): Position = {
      val newIndex = idx + numChars
      val substr = text.substring(0, newIndex)
      val newRow = 1 + substr.count(_ == '\n')
      val newCol = newIndex - substr.lastIndexOf('\n')
      Position(newIndex, newRow, newCol)
    }
  }

  // map of token types to their scanning functions
  val transitions
      : Map[State.Type, (Position) => (Position, State.Type, Token)] = Map(
    State.Space -> ((curPos: Position) => {
      def transition(): State.Type = {
        def opState(str: String): State.Type = {
          return str match {
            case s if Sets.ArithOps.contains(s) => return State.ArithOp
            case s if Sets.RelOps.contains(s)   => return State.RelOp
            case s if Sets.EqOps.contains(s)    => return State.EqOp
            case s if Sets.CondOps.contains(s)  => return State.CondOp
            case s if Sets.LogOps.contains(s)   => return State.LogOp
            case s if Sets.CompOps.contains(s)  => return State.CompOp
            case s if Sets.AssOps.contains(s)   => return State.AssOp
            case s if Sets.IncOps.contains(s)   => return State.IncOp
          }
        }
        if (curPos.idx + 1 < text.length) {
          val substr = text.substring(curPos.idx, curPos.idx + 2)
          substr match {
            case "//"                      => return State.Single
            case "/*"                      => return State.Multi
            case "0x"                      => return State.Hex
            case s if Sets.Ops.contains(s) => return opState(s)
            case _                         => // do nothing
          }
        }
        if (curPos.idx < text.length) {
          val str = text(curPos.idx).toString()
          str match {
            case "\""                           => return State.Str
            case "\'"                           => return State.Char
            case s if s.matches(Patterns.Alpha) => return State.Id
            case s if s.matches(Patterns.Digit) => return State.Decimal
            case s if Sets.Marks.contains(s)    => return State.Mark
            case s if Sets.Ops.contains(s)      => return opState(s)
            case _ => throw new Exception(s"$curPos unexpected character: $str")
          }
        }
        return State.Space
      }
      val newState = transition()
      val token: Token = if (newState == State.Space) EOF else WIP
      (curPos, newState, token)
    }),
    State.Single -> ((curPos: Position) => {
      def build(): Position = {
        val substr = text.substring(curPos.idx, curPos.idx + 2)
        assert(substr == "//", s"$curPos expected '//', found $substr")
        var newPos = curPos.eat(2)
        while (newPos.idx < text.length) {
          val str = text(newPos.idx).toString()
          newPos = newPos.eat(1)
          if (str == "\n") {
            return newPos
          }
        }
        // EOL may end single-line comment instead of newline
        return newPos
      }
      val newPos = build()
      (newPos, State.Space, WIP)
    }),
    State.Multi -> ((curPos: Position) => {
      def build(): Position = {
        val substr = text.substring(curPos.idx, curPos.idx + 2)
        assert(substr == "/*", s"$curPos expected '/*', found $substr")
        var newPos = curPos.eat(2)
        while (newPos.idx + 1 < text.length) {
          val substr = text.substring(newPos.idx, newPos.idx + 2)
          if (substr == "*/") {
            newPos = newPos.eat(2)
            return newPos
          }
          newPos = newPos.eat(1)
        }
        throw new Exception(s"$newPos unclosed multiline comment")
      }
      val newPos = build()
      (newPos, State.Space, WIP)
    }),
    State.Str -> ((curPos: Position) => {
      def build(): Position = {
        val ch = text(curPos.idx)
        assert(ch == '\"', s"""$curPos expected ", found $ch""")
        var newPos = curPos.eat(1)
        var backslash = false
        while (newPos.idx < text.length) {
          val str = text(newPos.idx).toString()
          if (backslash) {
            // we just saw a backslash
            if (!Sets.Escaped.contains(str)) {
              throw new Exception(s"$newPos invalid escape sequence: $str")
            }
            backslash = false
          } else {
            val int = str(0).toInt
            if (str == "\\") {
              backslash = true
            } else if (str == "\n") {
              throw new Exception(s"$newPos newline in string literal")
            } else if (str == "\"") {
              // found closing double quote
              return newPos.eat(1)
            } else if (int < ASCII.First || int > ASCII.Last || str == "\'") {
              throw new Exception(s"$newPos invalid character: $str")
            }
          }
          newPos = newPos.eat(1)
        }
        throw new Exception(s"$newPos unclosed string")
      }
      val newPos = build()
      val token = new StrToken(
        text.substring(curPos.idx + 1, newPos.idx - 1),
        curPos.row,
        curPos.col
      )
      (newPos, State.Space, token)
    }),
    State.Char -> ((curPos: Position) => {
      def build(): Position = {
        val ch = text(curPos.idx)
        assert(ch == '\'', s"$curPos expected ', found $ch")
        var newPos = curPos.eat(1)
        var numChars = 0
        var isBackslash = false
        while (newPos.idx < text.length) {
          val str = text(newPos.idx).toString()
          if (isBackslash) {
            // we just saw a backslash
            if (!Sets.Escaped.contains(str)) {
              throw new Exception(s"$newPos invalid escape sequence: $str")
            }
            numChars += 1
            isBackslash = false
          } else {
            val int = str(0).toInt
            if (str == "\\") {
              isBackslash = true
            } else if (str == "\n") {
              throw new Exception(s"$newPos newline in character literal")
            } else if (str == "\'") {
              // found closing single quote
              if (numChars == 0) {
                throw new Exception(s"$newPos illegal empty character")
              }
              return newPos.eat(1)
            } else if (int < ASCII.First || int > ASCII.Last || str == "\"") {
              throw new Exception(s"$newPos invalid character: $str")
            } else {
              // valid ascii character
              if (numChars >= 1) {
                throw new Exception(s"$newPos expected ', found $str")
              } else {
                numChars += 1
              }
            }
          }
          newPos = newPos.eat(1)
        }
        throw new Exception(s"$newPos unclosed character")
      }
      val newPos = build()
      val token = new CharToken(
        text.substring(curPos.idx + 1, newPos.idx - 1),
        curPos.row,
        curPos.col
      )
      (newPos, State.Space, token)
    }),
    State.Decimal -> ((curPos: Position) => {
      def build(): Position = {
        val str = text(curPos.idx).toString
        assert(
          str.matches(Patterns.Digit),
          s"$curPos expected decimal digit, found $str"
        )
        var newPos = curPos.eat(1)
        while (newPos.idx < text.length) {
          val str = text(newPos.idx).toString()
          if (str.matches(Patterns.Digit)) {
            newPos = newPos.eat(1)
          } else {
            return newPos
          }
        }
        // EOL may end decimal number
        return newPos
      }
      val newPos = build()
      val token =
        new IntToken(
          text.substring(curPos.idx, newPos.idx),
          curPos.row,
          curPos.col
        )
      (newPos, State.Space, token)
    }),
    State.Hex -> ((curPos: Position) => {
      def build(): Position = {
        val substr = text.substring(curPos.idx, curPos.idx + 2)
        assert(substr == "0x", s"$curPos expected '0x', found $substr")
        var newPos = curPos.eat(2)
        while (newPos.idx < text.length) {
          val str = text(newPos.idx).toString()
          if (str.matches(Patterns.HexDigit)) {
            newPos = newPos.eat(1)
          } else {
            return newPos
          }
        }
        // EOL may end hexadecimal number
        return newPos
      }
      val newPos = build()
      val token =
        new IntToken(
          text.substring(curPos.idx, newPos.idx),
          curPos.row,
          curPos.col
        )
      (newPos, State.Space, token)
    }),
    State.Id -> ((curPos: Position) => {
      def build(): Position = {
        val str = text(curPos.idx).toString
        assert(
          str.matches(Patterns.Alpha),
          s"$curPos expected identifier, found $str"
        )
        var newPos = curPos.eat(1)
        while (newPos.idx < text.length) {
          val str = text(newPos.idx).toString()
          if (str.matches(Patterns.AlphaNum)) {
            newPos = newPos.eat(1)
          } else {
            return newPos
          }
        }
        // EOL may end identifier
        return newPos
      }
      val newPos = build()
      // check if identifier is a keyword
      val substr = text.substring(curPos.idx, newPos.idx)
      val token = if (Sets.Bools.contains(substr)) {
        new BoolToken(substr, curPos.row, curPos.col)
      } else if (Sets.Types.contains(substr)) {
        new TypeToken(substr, curPos.row, curPos.col)
      } else if (Sets.Keywords.contains(substr)) {
        new Keyword(substr, curPos.row, curPos.col)
      } else {
        new IdToken(substr, curPos.row, curPos.col)
      }
      (newPos, State.Space, token)
    }),
    State.Mark -> ((curPos: Position) => {
      val str = text(curPos.idx).toString
      assert(Sets.Marks.contains(str), s"$curPos expected mark, found $str")
      val newPos = curPos.eat(1)
      (newPos, State.Space, new Mark(str, curPos.row, curPos.col))
    }),
    State.ArithOp -> ((curPos: Position) => {
      val str = text(curPos.idx).toString
      assert(
        Sets.ArithOps.contains(str),
        s"$curPos expected arith op, found $str"
      )
      val newPos = curPos.eat(1)
      (newPos, State.Space, new ArithOp(str, curPos.row, curPos.col))
    }),
    State.RelOp -> ((curPos: Position) => {
      if (
        curPos.idx + 1 < text.length && Sets.RelOps
          .contains(text.substring(curPos.idx, curPos.idx + 2))
      ) {
        val newPos = curPos.eat(2)
        (
          newPos,
          State.Space,
          new RelOp(
            text.substring(curPos.idx, newPos.idx),
            curPos.row,
            curPos.col
          )
        )
      } else {
        val str = text(curPos.idx).toString
        assert(
          Sets.RelOps.contains(str),
          s"$curPos expected rel op, found $str"
        )
        val newPos = curPos.eat(1)
        (newPos, State.Space, new RelOp(str, curPos.row, curPos.col))
      }
    }),
    State.EqOp -> ((curPos: Position) => {
      val substr = text.substring(curPos.idx, curPos.idx + 2)
      assert(
        Sets.EqOps.contains(substr),
        s"$curPos expected eq op, found $substr"
      )
      val newPos = curPos.eat(2)
      (newPos, State.Space, new EqOp(substr, curPos.row, curPos.col))
    }),
    State.CondOp -> ((curPos: Position) => {
      val substr = text.substring(curPos.idx, curPos.idx + 2)
      assert(
        Sets.CondOps.contains(substr),
        s"$curPos expected cond op, found $substr"
      )
      val newPos = curPos.eat(2)
      (newPos, State.Space, new CondOp(substr, curPos.row, curPos.col))
    }),
    State.LogOp -> ((curPos: Position) => {
      val str = text(curPos.idx).toString
      assert(Sets.LogOps.contains(str), s"$curPos expected log op, found $str")
      val newPos = curPos.eat(1)
      (newPos, State.Space, new LogOp(str, curPos.row, curPos.col))
    }),
    State.CompOp -> ((curPos: Position) => {
      val substr = text.substring(curPos.idx, curPos.idx + 2)
      assert(
        Sets.CompOps.contains(substr),
        s"$curPos expected comp op, found $substr"
      )
      val newPos = curPos.eat(2)
      (newPos, State.Space, new CompOp(substr, curPos.row, curPos.col))
    }),
    State.AssOp -> ((curPos: Position) => {
      val str = text(curPos.idx).toString
      assert(Sets.AssOps.contains(str), s"$curPos expected ass op, found $str")
      val newPos = curPos.eat(1)
      (newPos, State.Space, new AssOp(str, curPos.row, curPos.col))
    }),
    State.IncOp -> ((curPos: Position) => {
      val substr = text.substring(curPos.idx, curPos.idx + 2)
      assert(
        Sets.IncOps.contains(substr),
        s"$curPos expected inc op, found $substr"
      )
      val newPos = curPos.eat(2)
      (newPos, State.Space, new IncOp(substr, curPos.row, curPos.col))
    })
  )

  // given a position and state, return the next position, state, and token
  def next(pos: Position, state: State.Type): (Position, State.Type, Token) = {
    var curPos = pos
    var curState = state
    var curToken: Token = WIP
    while (curToken == WIP) {
      if (state == State.Space) {
        // seek until the first non-whitespace character
        def seek(): Position = {
          var newPos = curPos
          while (newPos.idx < text.length) {
            val str = text(newPos.idx).toString()
            if (!str.matches(Patterns.Whitespace)) {
              return newPos
            }
            newPos = newPos.eat(1);
          }
          return newPos
        }
        curPos = seek()
      }

      val (newPos, newState, newToken) = transitions(curState)(curPos)
      curPos = newPos
      curState = newState
      curToken = newToken
    }
    (curPos, curState, curToken)
  }

  /** Scans the entire text `string` and returns a collection of all the tokens,
    * in order
    */
  def scan(): Seq[Token] = {
    var (curPos, curState, curToken) = next(Position(0, 1, 1), State.Space)
    var tokens = List[Token]()
    while (curToken != EOF) {
      tokens :+= curToken
      val (newPos, newState, newToken) = next(curPos, curState)
      curPos = newPos
      curState = newState
      curToken = newToken
    }
    tokens
  }
}
