package in.tamchow.turing

import scala.language.postfixOps

/**
  * Represents a universal Turing Machine
  */
object UniversalTuringMachine {
  /**
    * The comment indicator
    */
  val CommentChar = ";"
  /**
    * Directives indicating acceptable halting commands, other than the default halt command
    */
  val DirectiveChar = "#"
  /**
    * The initial state indicator
    */
  val InitialStateChar = "~"
  /**
    * The tape initializer indicator
    */
  val FillerChar = "@"
  /**
    * List of
    */
  val SpecialChars = Seq(CommentChar, DirectiveChar, InitialStateChar, FillerChar)
  val IllegalTapeSizeMessage = "Illegal Tape Size : "
  val HaltState, BlankValue = null
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
    * 3. Lines having a '[[CommentChar]]' (';') in the middle - Only the part to the left is processed,
    * rest is discarded as an inline comment
    *
    * 4. Line starting with '[[InitialStateChar]]' ('~') - The initial state indicator
    *
    * 5. Line starting with '[[FillerChar]]' ('@') - The tape initializer
    *
    * This `String` is split by whitespace after trimming indicator to get filler `String`s,
    * which are repeated in order to fill up the tape
    *
    * Without such a line, the tape is initialized to logical blanks, represented by `null`
    *
    * 6. Empty Lines - Ignored and discarded
    *
    * @param data     Raw `Seq` of `String`s of a Turing Machine Code program
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
    import StringUtils._
    /**
      * Checks if the `item` is a valid [[TuringCommand]]
      *
      * @param item the item to check for validity as a [[TuringCommand]]
      * @return whether the item is a command or not
      */
    def isCommand(item: String) = !(item.isEmpty || qualifierAnyOf(item, SpecialChars))
    /**
      * Checks if the `item` has a mid-line comment
      *
      * @param item the item to check for possession of a mid-line comment
      * @return whether the item has a mid-line comment or not
      */
    def hasMidLineComment(item: String) = (item indexOf CommentChar) > 0
    val commandsDataNoComments = for (item <- data if isCommand(item)) yield TuringCommand(item)
    val commandsDataWithComments = for (item <- data if hasMidLineComment(item)) yield
      TuringCommand(trim(item, CommentChar, toOrUntil = false))
    val commandsData = commandsDataNoComments ++ commandsDataWithComments
    val initialStateRaw = process(data, InitialStateChar).headOption
    val initialState = initialStateRaw match {
      case Some(state) => state
      case None => commandsData.head.currentState
    }
    val tapeFiller = if (data exists (qualifier(_, FillerChar))) Some(process(data, FillerChar)) else None
    val terminationData = if (data exists (qualifier(_, DirectiveChar))) {
      for (item <- data if qualifier(item, DirectiveChar)) yield trim(item, DirectiveChar)
    } else {
      Seq()
    }
    new UniversalTuringMachine(commandsDataNoComments ++ commandsDataWithComments, initialState, terminationData, tapeFiller, tapeSize)
  }
}

/**
  * Minor `String` utilities
  */
object StringUtils {
  /**
    * Filters and trims the relevant `String`s
    *
    * @param data       the data to process
    * @param filterChar the character to filter the data by
    * @return the processed data
    * @see [[doStartFilter()]]
    * @see [[trim()]]
    */
  def process(data: Seq[String], filterChar: String) = doStartFilter(data, filterChar) map { case item => trim(item, filterChar) }

  /**
    * Filters the data `String`s by their starting characters
    *
    * @param data       the data to filter
    * @param filterChar the character to filter the data by
    * @return the filtered data
    * @see [[qualifier()]]
    */
  def doStartFilter(data: Seq[String], filterChar: String) = data filter (qualifier(_, filterChar))

  /**
    * Checks whether the item possesses the supplied qualifier
    *
    * @param item the item to check for possession of valid qualifier
    * @param char the qualifier character
    * @return whether the `item` starts with the qualifier character `char`
    */
  def qualifier(item: String, char: String) = item startsWith char

  /**
    * Flexible substring implementation
    *
    * @param string    the `String` whose substring is required
    * @param trimItem  the `String` at which the substring is required
    * @param toOrUntil whether to return the substring of `string` before or after `trimItem`
    * @return the required substring
    */
  def trim(string: String, trimItem: String, toOrUntil: Boolean = true) = {
    val trimIdx = string indexOf trimItem
    if (toOrUntil) string drop trimIdx else string take trimIdx
  }.trim

  /**
    * Checks whether the item possesses any of the supplied qualifiers
    *
    * @param item  the item to check for possession of valid qualifiers
    * @param chars the qualifier characters
    * @return whether the `item` starts with any of the qualifier characters in `chars`
    * @see [[qualifier()]]
    */
  def qualifierAnyOf(item: String, chars: Seq[String]) = chars exists (qualifier(item, _))
}

/**
  * Represents an [[UniversalTuringMachine]] object
  *
  * @constructor Requires all parameters.
  * @param commands                     The list of possible commands
  * @param initialState                 The initial state of this [[UniversalTuringMachine]]
  * @param terminalStatesWithoutDefault The possible halting states of this [[UniversalTuringMachine]]
  * @param tapeFiller                   The initializer for the tape of this [[UniversalTuringMachine]]
  * @param tapeSize                     The length of the tape of this [[UniversalTuringMachine]]
  */
final class UniversalTuringMachine(val commands: Seq[TuringCommand], val initialState: String, val terminalStatesWithoutDefault: Seq[String], val tapeFiller: Option[Seq[String]], val tapeSize: Int) {

  import UniversalTuringMachine._

  /**
    * A `String` representation of this [[UniversalTuringMachine]] object
    */
  override val toString = s"${this.getClass.getName}[$initialState,$commands,$terminalStatesWithoutDefault,$tapeFiller,$tapeSize]"
  /**
    * A hash code for this [[UniversalTuringMachine]] object, derived from {toString()}
    *
    * @see [[toString]]
    */
  override val hashCode = toString.hashCode
  /**
    * Append the default halt state to the list of termination states
    */
  val terminalStates = terminalStatesWithoutDefault ++ Seq(HaltState)

  /**
    * Runs the program for the specified number of steps
    * or till the program halts (if the specified number of steps is negative)
    *
    * @param steps the number of steps for which the program is run.
    *              Supply negative values to run till halt
    * @return the history of the tape in each step as a `Seq` of `Seq`s of `String`s
    */
  def runForStepsOrTillHalt(steps: Int) = {
    val useStepping = steps >= 0
    /**
      * Runs one step of the program
      *
      * @param halt         indicates whether or not halt execution
      * @param currentSteps the current number of steps completed
      * @param head         the head index
      * @param state        the current state
      * @param tape         the current tape
      * @param tapeHistory  the tapes after individual steps
      * @return the tape history after completion of this step
      */
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
    case None => (Vector fill tapeSize) (BlankValue)
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
    * @param head  the tape head
    * @param state the current state
    * @param tape  the current tape of this [[UniversalTuringMachine]]
    * @return a [[StepData]] instance containing the data for the next step
    */
  def runStep(head: Option[Int], state: String, tape: Seq[String]) = {
    val applicableStates = commands filter (isApplicable(_, head, state, tape))
    if (head == InvalidHead || applicableStates.isEmpty || (terminalStates contains state)) {
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
    * Uses a `toString()` and `hashCode()` trick
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