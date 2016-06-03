package in.tamchow.turing

import scala.language.postfixOps

/**
  * Represents a universal Turing Machine
  */
object UniversalTuringMachine {
  val CommentChar = ";"
  val DirectiveChar = "#"
  val InitialStateChar = "~"
  val FillerChar = "@"
  val SpecialChars = Vector(CommentChar, DirectiveChar, InitialStateChar, FillerChar)
  val IllegalTapeSizeMessage = "Illegal Tape Size : "
  val HaltState = ""
  val InvalidHead = None
  val InitialHead = 0


  /**
    * Allowed characters with special meaning:
    *
    * 1. Lines starting with '[[DirectiveChar]]' ('#') - Directives indicating acceptable halting commands,
    * other than the default halt command
    *
    * 2. Lines starting with '[[CommentChar]]' (';') - Discarded as comments.
    * Well, it is a Turing Machine Code program after all, so the assembler style is employed
    *
    * 3. Lines having a '[[CommentChar]]' (';') in the middle - Only the part to the Left is processed,
    * rest is discarded as an inline comment
    *
    * 4. Line starting with '[[InitialStateChar]]' ('~') - The initial command indicator
    *
    * 5. Line starting with '[[FillerChar]]' ('@') - The tape initializer
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
    * @see [[DirectiveChar]]
    * @see [[InitialStateChar]]
    * @see [[CommentChar]]
    * @see [[FillerChar]]
    * @see [[TuringCommand.WhitespaceRegex]]
    */
  def apply(data: Seq[String], tapeSize: Int) = {
    require(tapeSize > 0, s"$IllegalTapeSizeMessage$tapeSize")
    def doStartFilter(filterChar: String) = data filter (qualifier(_, filterChar))
    def trim(string: String, trimItem: String, toOrUntil: Boolean = true) = {
      val trimIdx = string indexOf trimItem
      if (toOrUntil) string drop trimIdx else string take trimIdx
    }.trim
    def process(filterChar: String) = doStartFilter(filterChar) map { case item => trim(item, filterChar) }
    def qualifier(item: String, char: String) = item startsWith char
    def qualifierAnyOf(item: String, chars: Seq[String]) = chars exists (qualifier(item, _))
    def isCommand(item: String) = !(item.isEmpty || qualifierAnyOf(item, SpecialChars))
    def hasMidLineComment(item: String) = (item indexOf CommentChar) > 0
    val commandsDataNoComments = for (item <- data if isCommand(item)) yield TuringCommand(item)
    val commandsDataWithComments = for (item <- data if hasMidLineComment(item)) yield
      TuringCommand(trim(item, CommentChar, toOrUntil = false))
    val commandsData = commandsDataNoComments ++ commandsDataWithComments
    val initialStateRaw = process(InitialStateChar).headOption
    val initialState = initialStateRaw match {
      case Some(state) => state
      case None => commandsData.head.currentState
    }
    val tapeFiller = if (data exists (qualifier(_, FillerChar))) Some(process(FillerChar)) else None
    val terminationData = if (data exists (qualifier(_, DirectiveChar))) {
      {
        for (item <- data if qualifier(item, DirectiveChar)) yield trim(item, DirectiveChar)
      } ++ Seq(HaltState)
    } else {
      Seq(HaltState)
    }
    new UniversalTuringMachine(commandsDataNoComments ++ commandsDataWithComments, initialState, terminationData, tapeFiller, tapeSize)
  }
}

/**
  * Represents an [[UniversalTuringMachine]] object
  *
  * @constructor Requires all parameters.
  * @param commands       The list of possible commands
  * @param initialState   The initial state of this [[UniversalTuringMachine]]
  * @param terminalStates The possible halting states of this [[UniversalTuringMachine]]
  * @param tapeFiller     The initializer for the tape of this [[UniversalTuringMachine]]
  * @param tapeSize       The length of the tape of this [[UniversalTuringMachine]]
  */
final class UniversalTuringMachine(val commands: Seq[TuringCommand], val initialState: String, val terminalStates: Seq[String], val tapeFiller: Option[Seq[String]], val tapeSize: Int) {
  /**
    * A [[java.lang.String]] representation of this [[UniversalTuringMachine]] object
    */
  override val toString = this.getClass.getName + "[" + initialState + "," + commands + "," + terminalStates + "," + tapeFiller + "," + tapeSize + "]"
  /**
    * A hash code for this [[UniversalTuringMachine]] object, derived from {toString()}
    *
    * @see [[toString]]
    */
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
    def doRun(halt: Boolean, currentSteps: Int, head: Option[Int], state: String, tape: Seq[String], tapeHistory: Seq[Seq[String]]): Seq[Seq[String]] = {
      if (halt || (useStepping && currentSteps >= steps)) tapeHistory
      else {
        val StepData(continue, nextHead, nextState, nextTape) = runStep(head, state, tape)
        doRun(halt = !continue, currentSteps + 1, nextHead, nextState, nextTape, tapeHistory :+ tape)
      }
    }
    doRun(halt = false, 0, Some(InitialHead), initialState, initTape, Vector())
  }

  /**
    * Fills the tape initially with the [[tapeFiller]] contents repeatedly.
    *
    * @return a properly initialized tape
    */
  def initTape = tapeFiller match {
    case None => (Vector fill tapeSize) (null)
    case Some(filler) =>
      val (times, remainder) = (tapeSize / filler.length, tapeSize % filler.length)
      def fill(fillCount: Int, struct: Seq[String]): Seq[String] = {
        if (fillCount == 0) struct ++ (filler take remainder)
        else fill(fillCount - 1, struct ++ filler)
      }
      fill(times, Vector())
  }

  /**
    * Runs one step of a program
    *
    * @return true if there are more steps possible, else false
    */
  def runStep(head: Option[Int], state: String, tape: Seq[String]) = {
    val applicableStates = commands filter (isApplicable(_, head, state, tape))
    if (applicableStates.isEmpty || (terminalStates contains state)) {
      StepData(continue = false, InvalidHead, HaltState, tape)
    } else {
      val headIndex = head.getOrElse(InitialHead)
      val applicableState = applicableStates.head
      StepData(continue = true, Some(headIndex + applicableState.direction.value), applicableState.nextState, tape updated(bounded(headIndex), applicableState.nextValue))
    }
  }

  /**
    * Helper function for filtering available commands by applicability
    *
    * @param state   The current state of this [[UniversalTuringMachine]]
    * @param head    The current head pointer to the tape of this [[UniversalTuringMachine]]
    * @param command The command to check for applicability with the current execution environment
    * @return true if the command is applicable, false if it isn't
    */
  private[this] def isApplicable(command: TuringCommand, head: Option[Int], state: String, tape: Seq[String]) =
    head != InvalidHead && state != HaltState &&
      (command.currentValue == tape(bounded(head.getOrElse(InitialHead))) || command.valueMatchesEverything) &&
      (command.currentState == state || command.stateMatchesEveryThing)

  /**
    * Implements index wraparound about [[tapeSize]]
    *
    * @param head the argument to bounds-correct
    * @return the argument after bounds correction
    */
  private[this] def bounded(head: Int) =
    if (head < 0) Math.abs(tapeSize + head) % tapeSize else if (head >= tapeSize) head % tapeSize else head

  /**
    * Equality comparison of this [[UniversalTuringMachine]] object with the argument
    *
    * Uses a {toString()} and {hashCode()} trick
    *
    * @param other the object to compare this [[UniversalTuringMachine]] object for equality to
    * @return whether this [[UniversalTuringMachine]] equals {other}
    * @see [[hashCode]]
    * @see [[toString]]
    */
  override def equals(other: Any): Boolean = other match {
    case that: UniversalTuringMachine =>
      hashCode == that.hashCode
    case _ => false
  }
}

/**
  * Contains the data for one step in the run of this [[UniversalTuringMachine]]
  *
  * @param continue  an indicator whether to continue for the next step
  * @param nextHead  the tape head index after this step
  * @param nextState the state of this [[UniversalTuringMachine]] after this step
  * @param tape      the tape of this [[UniversalTuringMachine]] as soon as this step has completed
  */
final case class StepData(continue: Boolean, nextHead: Option[Int], nextState: String, tape: Seq[String])