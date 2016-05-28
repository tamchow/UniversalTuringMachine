package in.tamchow.turing

import scala.language.postfixOps

/**
  * Represents a universal Turing Machine
  */
object UniversalTuringMachine {
  val (commentChar, directiveChar, initialStateChar, fillerChar) = (";", "#", "~", "@")
  val specialChars = Vector(commentChar, directiveChar, initialStateChar, fillerChar)
  val illegalTapeSizeMessage = "Illegal Tape Size : "
  val (haltState, invalidHead, initialHead) = (None, None, Some(0))

  def toTuple(data: StepData) = StepData.unapply(data).get

  /**
    * Allowed characters with special meaning:
    *
    * 1. Lines starting with '[[directiveChar]]' ('#') - Directives indicating acceptable halting commands,
    * other than the default halt command
    *
    * 2. Lines starting with '[[commentChar]]' (';') - Discarded as comments.
    * Well, it is a Turing Machine Code program after all, so the assembler style is employed
    *
    * 3. Lines having a '[[commentChar]]' (';') in the middle - Only the part to the left is processed,
    * rest is discarded as an inline comment
    *
    * 4. Line starting with '[[initialStateChar]]' ('~') - The initial command indicator
    *
    * 5. Line starting with '[[fillerChar]]' ('@') - The tape initializer
    *
    * This [[java.lang.String]] is split by whitespace after trimming indicator to get filler [[java.lang.String]]s,
    * which are repeated in order to fill up the tape
    *
    * Without such a line, the tape is initialized to logical blanks, represented by `null`
    *
    * 6. Empty Lines - Ignored and discarded
    *
    * @param data     Raw [[scala.Vector]] of [[java.lang.String]]s of a Turing Machine Code program
    * @param tapeSize the (fixed) size of the tape. Wraparound is enabled
    * @return An [[UniversalTuringMachine]] object for evaluation of the argument program
    * @see [[directiveChar]]
    * @see [[initialStateChar]]
    * @see [[commentChar]]
    * @see [[fillerChar]]
    * @see [[TuringCommand.whitespaceRegex]]
    */
  def apply(data: Seq[String], tapeSize: Int) = {
    require(tapeSize > 0, s"$illegalTapeSizeMessage$tapeSize")
    def doStartFilter(filterChar: String) = data filter (qualifier(_, filterChar))
    def trim(string: String, trimItem: String, toOrUntil: Boolean = true) = {
      val trimIdx = string indexOf trimItem
      if (toOrUntil) string drop trimIdx else string take trimIdx
    }.trim
    def process(filterChar: String) = doStartFilter(filterChar) map { case item => trim(item, filterChar) }
    def qualifier(item: String, char: String) = item startsWith char
    def qualifierAnyOf(item: String, chars: Seq[String]) = chars exists (qualifier(item, _))
    def isCommand(item: String) = !(item.isEmpty || qualifierAnyOf(item, specialChars))
    def hasMidLineComment(item: String) = (item indexOf commentChar) > 0
    val commandsDataNoComments = for (item <- data if isCommand(item)) yield TuringCommand(item)
    val commandsDataWithComments = for (item <- data if hasMidLineComment(item)) yield
      TuringCommand(trim(item, commentChar, toOrUntil = false))
    val initialState = process(initialStateChar).headOption
    val tapeFiller: Seq[String] = if (data exists (qualifier(_, fillerChar))) process(fillerChar) else null
    val terminationData = for (item <- data if qualifier(item, directiveChar)) yield trim(item, directiveChar)
    new UniversalTuringMachine(commandsDataNoComments ++ commandsDataWithComments, initialState, terminationData, tapeFiller, tapeSize)
  }
}

final class UniversalTuringMachine(val commands: Seq[TuringCommand], val initialState: Option[String], val terminalStates: Seq[String], val tapeFiller: Seq[String], val tapeSize: Int) {
  override val toString = this.getClass.getName + "[" + initialState + "," + commands + "," + terminalStates + "," + tapeFiller + "," + tapeSize + "]"
  override val hashCode = toString.hashCode

  import UniversalTuringMachine._

  /**
    * Runs the program for the specified number of steps
    * or till the program halts (if the specified number of steps is negative)
    *
    * @param steps the number of steps for which the program is run.
    *              Supply negative values to run till halt
    * @return the history of the tape in each step as a [[scala.Seq]] of [[scala.Seq]]s of [[java.lang.String]]s
    */
  def runForStepsOrTillHalt(steps: Int) = {
    val useStepping = steps >= 0
    def doRun(halt: Boolean, currentSteps: Int, head: Option[Int], state: Option[String], tape: Seq[String], tapeHistory: Seq[Seq[String]]): Seq[Seq[String]] = {
      if (halt || (useStepping && currentSteps >= steps)) tapeHistory
      else {
        val (continue, nextHead, nextState, nextTape) = toTuple(runStep(head, state, tape))
        doRun(halt = !continue, currentSteps + 1, nextHead, nextState, nextTape, tapeHistory :+ tape)
      }
    }
    doRun(halt = false, 0, initialHead, initialState, initTape, Vector())
  }

  /**
    * Fills the tape initially with the [[tapeFiller]] contents repeatedly.
    *
    * @return a properly initialized tape
    */
  def initTape = {
    if (tapeFiller == null) (Vector fill tapeSize) (null)
    else {
      val (times, remainder) = (tapeSize / tapeFiller.length, tapeSize % tapeFiller.length)
      def fill(fillCount: Int, struct: Seq[String]): Seq[String] = {
        if (fillCount == 0) struct ++ (tapeFiller take remainder)
        else fill(fillCount - 1, struct ++ tapeFiller)
      }
      fill(times, Vector())
    }
  }

  /**
    * Runs one step of a program
    *
    * @return true if there are more steps possible, else false
    */
  def runStep(head: Option[Int], state: Option[String], tape: Seq[String]) = {
    val applicableStates = commands filter (isApplicable(_, head, state, tape))
    if (applicableStates.isEmpty || state == haltState || (terminalStates contains state.get)) {
      StepData(continue = false, invalidHead, haltState, tape)
    } else StepData(continue = true, Some(applicableStates.head.direction match {
      case MoveDirection.left => head.get - 1
      case MoveDirection.right => head.get + 1
      case MoveDirection.none => head.get
      case other => throw new IllegalArgumentException(MoveDirection.illegalTypeMessage format (other getClass).getName)
    }), Some(applicableStates.head.nextState), tape updated(bounded(head.get), applicableStates.head.nextValue))
  }

  /**
    * Helper function for filtering available commands by applicability
    *
    * @param state   The current state of this [[UniversalTuringMachine]]
    * @param head    The current head pointer to the tape of this [[UniversalTuringMachine]]
    * @param command The command to check for applicability with the current execution environment
    * @return true if the command is applicable, false if it isn't
    */
  private[this] def isApplicable(command: TuringCommand, head: Option[Int], state: Option[String], tape: Seq[String]) = {
    head != invalidHead && state != haltState &&
      (command.currentValue == tape(bounded(head.get)) || (command valueMatchesEverything)) &&
      (command.currentState == state.get || (command stateMatchesEveryThing))
  }

  /**
    * Implements [[Array]] wraparound
    *
    * @param head the argument to bounds-correct
    * @return the argument after bounds correction
    */
  private[this] def bounded(head: Int) =
    if (head < 0) Math.abs(tapeSize + head) % tapeSize else if (head >= tapeSize) head % tapeSize else head

  override def equals(other: Any): Boolean = other match {
    case that: UniversalTuringMachine =>
      hashCode == that.hashCode
    case _ => false
  }
}

final case class StepData(continue: Boolean, nextHead: Option[Int], nextState: Option[String], tape: Seq[String])