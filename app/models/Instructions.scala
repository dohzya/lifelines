package lifelines
package models
package instructions

import scala.concurrent.duration.FiniteDuration

sealed trait Instruction
case class Wait(duration: FiniteDuration, instr: Instruction) extends Instruction
case class Talk(content: String) extends Instruction
case class Info(content: String) extends Instruction
case class Question(choices: Map[String, String]) extends Instruction
case class SetCtx(param: String, value: Int) extends Instruction
case class IncrCtx(param: String, value: Int) extends Instruction
case class DecrCtx(param: String, value: Int) extends Instruction
case class IfCtxEQ(param:  String, value: Int, instr: Instruction) extends Instruction
case class IfCtxGT(param: String, value: Int, instr: Instruction) extends Instruction
case class IfCtxGTE(param: String, value: Int, instr: Instruction) extends Instruction
case class IfCtxLT(param: String, value: Int, instr: Instruction) extends Instruction
case class IfCtxLTE(param: String, value: Int, instr: Instruction) extends Instruction
case class Jump(step: String) extends Instruction
