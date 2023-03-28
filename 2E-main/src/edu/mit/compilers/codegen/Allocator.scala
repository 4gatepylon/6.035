package edu.mit.compilers.codegen

// Allocates registers to have locations on the stack or data
// Right now this just means that it gives them an "offset" value
object RegAllocator {
  val stackDir: Long = -1
  val stackStride: Long = 8
  var stackPointer: Long = -8
  var paramStackPointer: Long = 16

  var stringLabelCounter: Int = 0
  var dataLabelCounter: Int = 0

  // This is filled up as we create string registers
  // and it's used in codegen to create the data segment
  var stringToLabel: Map[String, StringLabel] = Map()
  var dataVarNameToLabel: Map[String, DataLabel] = Map()
  var regToLoc: Map[Register, Loc] = Map()

  def getLoc(reg: Register): Loc = {
    reg match {
      case ArrElemAddr(base, index, _, _) => {
        // The index's VALUE (not mem loc) is used for this
        ArrAddrOffset(
          getLoc(base).asInstanceOf[StaticMemLoc[Long]],
          DynamicOffset(index)
        )
      }
      case _ => {
        regToLoc
          .get(reg)
          .getOrElse(
            throw new Exception(
              s"Failed to find register ${reg} in regToLoc"
            )
          )
      }
    }
  }

  def hasLoc(reg: Register): Boolean = {
    reg match {
      case ArrElemAddr(base, index, _, _) => hasLoc(base) && hasLoc(index)
      case _                              => regToLoc.contains(reg)
    }
  }

  // Call this every time you want to generate code for a new CFG (method)
  def resetStack(): Unit = {
    stackPointer = -8
    paramStackPointer = 16
  }

  def pushStack(numEntries: Long = 1): Unit = {
    stackPointer += (stackDir * stackStride) * numEntries
  }
  def pushParamStack(numEntries: Long = 1): Unit = {
    // A bit of a misnomer, but basically this is pushStack but for paramStackPointer,
    // which goes in the positive direction (opposite of regular growth)
    paramStackPointer += (-stackDir * stackStride) * numEntries
  }

  def scalarReg(reg: ScalarAddr): Unit = {
    reg.location match {
      case AddrLocation.Stack => {
        val offset = StaticOffset(stackPointer)
        regToLoc += (reg -> offset)
        pushStack()
      }
      case AddrLocation.Data => {
        val label = DataLabel(dataLabelCounter, None)
        val varName = reg.varNameOpt.getOrElse(
          throw new Exception(
            "Data variable cannot lack a varName because Data variables are not temps"
          )
        )
        regToLoc += (reg -> label)
        dataVarNameToLabel += (varName -> label)
        dataLabelCounter += 1
      }
    }
  }

  def tmpScalarParamReg(reg: ScalarAddr): Unit = {
    val offset = StaticOffset(paramStackPointer)
    regToLoc += (reg -> offset)
    pushParamStack()
  }

  def strReg(reg: StrAddr): Unit = {
    val maybeStrLabel = stringToLabel.get(reg.value)
    if (maybeStrLabel.isEmpty) {
      // Strings use two maps... might want to change this
      val label = StringLabel(stringLabelCounter)
      stringToLabel += (reg.value -> label)
      regToLoc += (reg -> label)
      stringLabelCounter += 1
    } else {
      // Necessary (for now) for codeGen to find the string for different registers
      // with the same offset (thereby same underlying value)
      regToLoc += (reg -> maybeStrLabel.get)
    }
  }

  def varArrayBaseReg(reg: ArrBaseAddr): Unit = {
    reg.location match {
      case AddrLocation.Stack => {
        // First element stores the length and the rest are the values
        // Unclear why this must be called first for correctness
        pushStack(reg.length + 1)
        val offset = StaticOffset(stackPointer + stackStride)
        regToLoc += (reg -> offset)
      }
      case AddrLocation.Data => {
        val label = DataLabel(dataLabelCounter, Some(reg.length))
        regToLoc += (reg -> label)
        dataVarNameToLabel += (reg.varName -> label)
        dataLabelCounter += 1
      }
      case l => throw new Exception(s"Unsupported array storage location: $l")
    }
  }

  def tmpArrayIndexReg(reg: ArrElemAddr): Unit = {
    // Offset is implicit in the base and index's offsets
    reg.base match {
      case b: ArrBaseAddr => {

        // Skips the first entry (first 16 bytes) which is a length
        // Array base offsets are always static
        val baseOffset = regToLoc
          .get(reg.base)
          .getOrElse(
            throw new Exception(
              s"Cannot instantiate an offset for ArrelemAddr if base has no offset: ${reg.base}"
            )
          )
          .asInstanceOf[StaticMemLoc[Long]]
        val offset: ArrAddrOffset =
          ArrAddrOffset(
            baseOffset,
            DynamicOffset(reg.index)
          )
        regToLoc += (reg -> offset)
      }
      case _ =>
        throw new Exception(
          "Trying to make temp array index reg, but base provided is not array base"
        )
    }
  }

  def alloc(reg: Register): Unit = {
    reg match {
      case s: ScalarAddr => {
        // Parameters have already been allocated using the paramRegs list
        // from the regTable in the starting BB of the CFG by the CFGAllocator.
        scalarReg(s)
      }
      case s: StrAddr     => strReg(s)
      case s: ArrBaseAddr => varArrayBaseReg(s)
      case s: ArrElemAddr => tmpArrayIndexReg(s)
      case _ =>
        throw new Exception(
          s"Unsupported register type: $reg for `alloc` in `RegAllocator`"
        )
    }
  }
}

// Allocates registers given instructions
object InstrAllocator {
  def argsIfStrings(instr: BasicInstr): Unit = {
    instr match {
      case u: UnOpInstr => {
        if (u.arg.isInstanceOf[StrAddr]) {
          RegAllocator.alloc(u.arg)
        }
      }
      case b: BinOpInstr => {
        if (b.arg1.isInstanceOf[StrAddr]) {
          RegAllocator.alloc(b.arg1)
        }
        if (b.arg2.isInstanceOf[StrAddr]) {
          RegAllocator.alloc(b.arg2)
        }
      }
      case c: CallInstr => {
        // You cannot pass in things you have not allocated assuming it's
        // meant to be in memory.
        c.args
          .filter(_.isInstanceOf[StrAddr])
          .foreach(arg => RegAllocator.alloc(arg))
      }
      case _ => // NOTE: you can't return strings, declare strings, etc...
    }
  }

  def assertArgsAllocated(instr: BasicInstr): Unit = {
    val args: List[Register] = instr match {
      case r: UnOpInstr  => List(r.arg)
      case r: RetInstr   => List(r.arg)
      case r: CopyInstr  => List(r.arg)
      case r: BinOpInstr => List(r.arg1, r.arg2)
      case r: CallInstr  => r.args
      case r: DeclInstr => {
        // Declarations should be the first time you see a register, so it should NOT
        // already have a Loc.
        if (RegAllocator.hasLoc(r.reg)) {
          val ml = RegAllocator.getLoc(r.reg)
          throw new Exception(
            s"DeclInstr Instruction $r has reg ${r.reg} ALREADY allocated with a Loc ${ml}"
          )
        }
        // Nothing to check
        List()
      }
      case _ => List()
    }

    // Obviously built in registers cannot be allocated to MEMORY
    // and nor can constants. ArrElemAddr have built in Locs from
    // from the base and index.
    args
      .filterNot(RegLocInfo.isBuiltIn(_))
      .filterNot(_.isInstanceOf[ConstReg])
      .filterNot(_.isInstanceOf[ArrElemAddr])
      .foreach(arg => {
        assert(
          RegAllocator.hasLoc(arg),
          s"Instruction $instr has arg ${arg} with no allocated Loc"
        )
      })
  }

  def alloc(instr: BasicInstr, table: RegTable): Unit = {
    // By convention, you can write into somewhere that has a location or somewhere new
    // (we will allocate below), but you cannot read from somewhere that is not allocated.
    // However, string constants are declared in-line, so they cannot have been pre-allocated
    // by the `GlobalVarAllocator` (below). For that reason we only allocate string arguments.
    argsIfStrings(instr)
    assertArgsAllocated(instr)

    // If it's not a decl then allocate the destination, if it is not
    // null (look in Instruction.scala) and not already allocated (i.e.
    // variable's or param's register). If it was a decl, then you need to allocate
    // `reg` in that instruction.
    instr match {
      case DeclInstr(reg, size) => RegAllocator.alloc(reg)
      case destInstr: DestInstr => {
        val dest = destInstr.dest
        if (!RegLocInfo.isBuiltIn(dest) && !RegAllocator.hasLoc(dest)) {
          RegAllocator.alloc(dest)
        }
      }
      case _ =>
    }
  }
}

// Allocates registers given CFGs
object CFGAllocator {
  // This will probably be deprecated soon
  object MaxRspTable {
    var methodToMaxRsp: Map[String, Long] = Map()
    def get(method: String): Long = {
      methodToMaxRsp.getOrElse(method, 0)
    }
    def set(method: String, maxRsp: Long): Unit = {
      methodToMaxRsp += (method -> (maxRsp - (maxRsp % 16)))
    }
  }

  // NOTE: this ONLY allocates for a SINGLE method
  // NOTE: `cfg` must be the first block of the method.
  def alloc(methodName: String, cfg: BasicBlock): BasicBlock = {
    // Make sure the stack starts at 0
    RegAllocator.resetStack()

    // Use the table of the first BB to allocate the parameters
    val table: RegTable = cfg.regTable.getOrElse(
      throw new Exception(
        s"First basic block for method `$methodName` must have a register table for `alloc` in `CFGAllocator`"
      )
    )
    // Not sure why this is the convention that remained.
    var paramsTable: RegTable = table.parent.getOrElse(
      throw new Exception(
        s"First basic block's table for method `$methodName` must have a parent table for `alloc` in `CFGAllocator`." +
          " NOTE that the table's parent is the paramsTable"
      )
    )
    // Allocate parameters first
    paramsTable.paramRegs
      .filterNot(RegLocInfo.isBuiltIn(_))
      .foreach(r => {
        RegAllocator.tmpScalarParamReg(r.asInstanceOf[ScalarAddr])
      })

    // Use the cfg's visitor to allocate any other variables (NOTE: this probably
    // relies on BFS and might fail for DFS because a variable may not be defined in time)
    cfg.visit[Unit](bb =>
      bb.instrs.foreach(instr =>
        InstrAllocator.alloc(
          instr,
          bb.regTable.getOrElse(
            throw new Exception(
              s"BB $bb has no register table for `alloc` in `CFGAllocator` forEach of instructions using visitor"
            )
          )
        )
      )
    )

    // Set the MaxRspTable
    CFGAllocator.MaxRspTable.set(methodName, RegAllocator.stackPointer)

    cfg
  }
}

// Allocates all the global (Data) variables, strings, etcetera
object GlobalVarAllocator {
  // Allocate using the global table
  def alloc(table: RegTable): Unit = {
    assert(
      table.parent.isEmpty,
      "GlobalVarAllocator should not have a parent table"
    )
    // Allocate all global variables
    for ((name, reg) <- table.nameToRegister) {
      RegAllocator.alloc(reg)
    }
  }
}
