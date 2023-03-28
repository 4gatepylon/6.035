package edu.mit.compilers.optimization

import edu.mit.compilers.codegen._

case object AlgSimp extends Optimization {
  // NOTE: this is copied from Checkers.scala from IntLiteralSize

  // AlgSimp will do the following
  // 1. It will replace expressions of constants with the constants they operate on
  // 2. It will replace x + 0 with x, x * 1 with x, and so forth (on both sides)
  // 3. It will replace x || true with true, x && false with false, and so forth (on both sides)
  // 4. --c with c (for constant -c)

  // NOTE that any divide by zero errors should be caught at runtime, not compile time.
  // NOTE that char constants are not supported yet.

  // In the future we may want to consider things like
  // 1. x - x = 0
  // 2. x == x = true, x != x = false (currently only done for constants)
  // 3. ...
  // 4. Strength Reduction: x * 2^k = x << k (we will need to add a new basic instruction)
  // 5. increment/decrement instructions

  // NOTE this has to be implemented by the children
  def algSimp(instr: BasicInstr): BasicInstr = {
    var algSimps = List[AlgSimpOpt](
      AlgSimpUnOp,
      AlgSimpEquals,
      AlgSimpCmp,
      AlgSimpBoolArith,
      AlgSimpIntArith,
      AlgSimpStrengthReduce
    )
    // apply algebraic simplifications from left to right
    algSimps.foldLeft(instr)((instr, algSimp) => algSimp.apply(instr))
  }

  def algSimpBlock(cfg: BasicBlock): BasicBlock = {
    val optCfg = cfg
    optCfg.instrs = optCfg.instrs.map(algSimp)
    optCfg
  }

  override def opt(cfg: BasicBlock): BasicBlock = {
    assert(
      cfg.functionHeader.getOrElse(
        throw new Exception("Provide function header to `AlgSimp.opt`")
      ) == cfg,
      "Need to be given function header (CFG) to `AlgSimp.opt`"
    )
    cfg.visit[BasicBlock](algSimpBlock)
    cfg
  }

  override def toString(): String = "algsimp"

  override def precedence(): Int = Precedences.ALG_SIMP
}

trait AlgSimpOpt {
  def isConstant(reg: Register): Boolean = reg match {
    case int: IntConstReg   => true
    case bool: BoolConstReg => true
    // Char Constants not supported
    case _ => false
  }

  def inLongRange(test: BigInt): Boolean =
    test >= BigInt(Long.MinValue) && test <= BigInt(Long.MaxValue)

  def apply(instr: BasicInstr): BasicInstr
}

case object AlgSimpStrengthReduce extends AlgSimpOpt {
  // assumes that integer arithmetic simplification is already done
  def isPowerOfTwo(x: Long): Boolean = x != 0 && (x & (x - 1)) == 0

  def log2(x: Long): Int = {
    var i = 0
    var y = x
    while (y > 1) {
      y = y >> 1
      i += 1
    }
    i
  }

  def apply(instr: BasicInstr): BasicInstr = instr match {
    case MulInstr(dest, arg1, arg2) =>
      (arg1, arg2) match {
        case (IntConstReg(n1), _) if isPowerOfTwo(n1) =>
          LeftShiftInstr(dest, arg2, IntConstReg(log2(n1)))
        case (_, IntConstReg(n2)) if isPowerOfTwo(n2) =>
          LeftShiftInstr(dest, arg1, IntConstReg(log2(n2)))
        case _ => instr
      }
    case DivInstr(dest, arg1, arg2) =>
      (arg1, arg2) match {
        case (_, IntConstReg(n2)) if isPowerOfTwo(n2) =>
          RightShiftInstr(dest, arg1, IntConstReg(log2(n2)))
        case _ => instr
      }
    case _ => instr
  }
}

case object AlgSimpEquals extends AlgSimpOpt {
  def apply(instr: BasicInstr): BasicInstr = instr match {
    case instr: CmpInstr => {
      val dest = instr.dest
      if (isConstant(instr.arg1) && isConstant(instr.arg2)) {
        val arg1: ConstReg = instr.arg1.asInstanceOf[ConstReg]
        val arg2: ConstReg = instr.arg2.asInstanceOf[ConstReg]
        instr match {
          case _: EqInstr =>
            CopyInstr(instr.dest, BoolConstReg(arg1.value == arg2.value))
          case _: NeInstr =>
            CopyInstr(instr.dest, BoolConstReg(arg1.value != arg2.value))
          case _ =>
            instr
        }
      }
      // NOTE: unclear this works because of the whole `id` debacle
      // else if (instr.arg1 == instr.arg2) {
      //   instr match {
      //     case _: EqInstr =>
      //       CopyInstr(instr.dest, BoolConstReg(true))
      //     case _: NeInstr =>
      //       CopyInstr(instr.dest, BoolConstReg(false))
      //     case _ =>
      //       throw new Exception(
      //         s"Unsupported instruction $instr in algSimpEq"
      //       )
      //   }
      // }
      else {
        instr
      }
    }
    case _ => instr
  }
}

case object AlgSimpCmp extends AlgSimpOpt {
  def apply(instr: BasicInstr): BasicInstr = instr match {
    case instr: CmpInstr => {
      instr.arg1 match {
        case arg1: IntConstReg => {
          instr.arg2 match {
            case arg2: IntConstReg => {
              instr match {
                case GtInstr(dest, _, _) =>
                  CopyInstr(dest, BoolConstReg(arg1.value > arg2.value))
                case LtInstr(dest, _, _) =>
                  CopyInstr(dest, BoolConstReg(arg1.value < arg2.value))
                case GeInstr(dest, _, _) =>
                  CopyInstr(dest, BoolConstReg(arg1.value >= arg2.value))
                case LeInstr(dest, _, _) =>
                  CopyInstr(dest, BoolConstReg(arg1.value <= arg2.value))
                case _ =>
                  instr
              }
            }
            case _ =>
              instr
          }
        }
        case _ =>
          instr
      }
    }
    case _ =>
      instr
  }
}

case object AlgSimpBoolArith extends AlgSimpOpt {
  def apply(instr: BasicInstr): BasicInstr = instr match {
    case instr: ArithInstr =>
      instr.arg1 match {
        case arg1: BoolConstReg =>
          instr.arg2 match {
            case arg2: BoolConstReg =>
              // Both arg1 and arg2
              instr match {
                case _: AndInstr =>
                  CopyInstr(instr.dest, BoolConstReg(arg1.value && arg2.value))
                case _: OrInstr =>
                  CopyInstr(instr.dest, BoolConstReg(arg1.value || arg2.value))
                case _ => instr
              }
            case _ =>
              // Only arg1
              if (arg1.value)
                instr match {
                  case instr: AndInstr => instr
                  case instr: OrInstr =>
                    CopyInstr(
                      instr.dest,
                      BoolConstReg(true)
                    )
                  case _ => instr
                }
              else
                instr match {
                  case instr: AndInstr =>
                    CopyInstr(
                      instr.dest,
                      BoolConstReg(false)
                    )
                  case instr: OrInstr => instr
                  case _              => instr
                }
          }
        case _ =>
          instr.arg2 match {
            case arg2: BoolConstReg =>
              // Only arg1
              if (arg2.value)
                instr match {
                  case instr: AndInstr => instr
                  case instr: OrInstr =>
                    CopyInstr(instr.dest, BoolConstReg(true))
                  case _ => instr
                }
              else
                instr match {
                  case instr: AndInstr =>
                    CopyInstr(instr.dest, BoolConstReg(false))
                  case instr: OrInstr => instr
                  case _              => instr
                }
            // Neither arg1 nor arg2
            case _ => instr
          }
      }
    case _ => instr
  }
}

case object AlgSimpIntArith extends AlgSimpOpt {
  def apply(instr: BasicInstr): BasicInstr = instr match {
    case instr: ArithInstr => {
      instr.arg1 match {
        case arg1: IntConstReg => {
          instr.arg2 match {
            case arg2: IntConstReg => {
              val arg1_bi: BigInt = BigInt(arg1.value)
              val arg2_bi: BigInt = BigInt(arg2.value)
              // Both
              instr match {
                case AddInstr(dest, _, _) => {
                  val sum_bi: BigInt = arg1_bi + arg2_bi
                  if (inLongRange(sum_bi)) {
                    CopyInstr(dest, IntConstReg(sum_bi.toLong))
                  } else {
                    instr
                  }
                }
                case SubInstr(dest, _, _) => {
                  val diff_bi: BigInt = arg1_bi - arg2_bi
                  if (inLongRange(diff_bi)) {
                    CopyInstr(dest, IntConstReg(diff_bi.toLong))
                  } else {
                    instr
                  }
                }
                case MulInstr(dest, _, _) => {
                  val prod_bi: BigInt = arg1_bi * arg2_bi
                  if (inLongRange(prod_bi)) {
                    CopyInstr(dest, IntConstReg(prod_bi.toLong))
                  } else {
                    instr
                  }
                }
                case DivInstr(dest, _, _) => {
                  if (arg2_bi.toLong != 0) {
                    val quot_bi: BigInt = arg1_bi / arg2_bi
                    if (inLongRange(quot_bi)) {
                      CopyInstr(dest, IntConstReg(quot_bi.toLong))
                    } else {
                      instr
                    }
                  } else {
                    instr
                  }
                }
                case ModInstr(dest, _, _) => {
                  if (arg2_bi.toLong != 0) {
                    val mod_bi: BigInt = arg1_bi % arg2_bi
                    if (inLongRange(mod_bi)) {
                      CopyInstr(dest, IntConstReg(mod_bi.toLong))
                    } else {
                      instr
                    }
                  } else {
                    instr
                  }
                }
                case _ =>
                  instr
              }
            }
            case _ => {
              // Only arg1
              arg1.value match {
                case 0L => {
                  instr match {
                    // 0 + x => x
                    case AddInstr(dest, _, arg2) =>
                      CopyInstr(dest, arg2)
                    // 0 - x => -x
                    // NOTE |maxInt| < |minInt|
                    case SubInstr(dest, _, arg2) =>
                      NegInstr(dest, arg2)
                    case _ =>
                      instr
                  }
                }
                case 1L => {
                  instr match {
                    // 1 * x => x
                    case MulInstr(dest, _, arg2) =>
                      CopyInstr(dest, arg2)
                    case _ =>
                      instr
                  }
                }
                case _ => {
                  // n * x or something like that cannot be iproved upon for now (later, strength reduction)
                  instr
                }
              }
            }
          }
        }
        case _ => {
          instr.arg2 match {
            case arg2: IntConstReg => {
              arg2.value match {
                case 0L => {
                  instr match {
                    // x + 0 => x
                    case AddInstr(dest, arg1, _) =>
                      CopyInstr(dest, arg1)
                    // x - 0 => x
                    case SubInstr(dest, arg1, _) =>
                      CopyInstr(dest, arg1)
                    // n + 3 or something like that cannot be improved upon here yet
                    case _ => {
                      instr
                    }
                  }
                }
                case 1L => {
                  instr match {
                    // x * 1 => x
                    case MulInstr(dest, arg1, _) =>
                      CopyInstr(dest, arg1)
                    // x / 1 => x
                    case DivInstr(dest, arg1, _) =>
                      CopyInstr(dest, arg1)
                    // n * 5 or something like that cannot be improved upon
                    // yet (will come into play during strength reduction)
                    case _ =>
                      instr
                  }
                }
                case _ =>
                  instr
              }
            }
            // Neither
            case _ =>
              instr
          }
        }
      }
    }
    case _ =>
      instr
  }
}

case object AlgSimpUnOp extends AlgSimpOpt {
  def apply(instr: BasicInstr): BasicInstr = instr match {
    case NegInstr(dest, arg1) => {
      if (isConstant(arg1)) {
        val arg1_val: Long = arg1.asInstanceOf[IntConstReg].value
        if (inLongRange(-BigInt(arg1_val))) {
          CopyInstr(dest, IntConstReg(-arg1_val))
        } else {
          instr
        }
      } else {
        instr
      }
    }
    case NotInstr(dest, arg1) => {
      if (isConstant(arg1)) {
        CopyInstr(dest, BoolConstReg(!arg1.asInstanceOf[BoolConstReg].value))
      } else {
        instr
      }
    }
    case _ =>
      instr
  }
}
