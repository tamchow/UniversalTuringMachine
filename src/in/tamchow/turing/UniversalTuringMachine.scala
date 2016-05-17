package in.tamchow.turing

import scala.language.postfixOps

/**
  * Represents a universal Turing Machine
  */
class UniversalTuringMachine(commands: Vector[TuringCommand], initialState: String, terminalStates: Vector[String], tapeFiller: Vector[String], tapeSize: Int) {

  private val _tape = initTape

  private var (head, state) = (0, initialState)

  /**
    * @note This is the best partially functional method of filling up a list repeatedly using the elements of another
    *       I could come up with. Please suggest a more idiomatic way if possible
    * @return a properly initialized tape
    */
  def initTape = {
    var fillerListIdx = 0
    def getFiller = {
      if (fillerListIdx > tapeFiller.size) fillerListIdx = 0
      fillerListIdx += 1
      tapeFiller(fillerListIdx)
    }
    def fill(item: String) = Array.fill(tapeSize)(item)
    if (tapeFiller == null) fill(null) else fill(getFiller)
  }

  /**
    * Runs the program for the specified number of steps
    * or till the program halts (if the specified number of steps is negative)
    *
    * @param steps the number of steps for which the program is run.
    *              Supply negative values to run till halt
    * @return the history of the tape in each step as a [[Vector]] of [[Array]]s of [[String]]s
    */
  def runForStepsOrTillHalt(steps: Int) = {
    var currentSteps = 0
    var halt = initialState == null
    val useStepping = steps >= 0
    var tapeHistory: List[Array[String]] = Nil
    while (!(halt || (useStepping && currentSteps >= steps))) {
      tapeHistory = tapeHistory :+ (tape clone())
      halt = runStep()
      currentSteps += 1
    }
    tapeHistory toVector
  }

  /**
    * Runs one step of a program
    *
    * @return true if there are more steps possible, else false
    */
  def runStep() = {
    val applicableStates = commands filter (isApplicable(_))
    if (applicableStates.isEmpty || state == null || (terminalStates contains state)) false
    else {
      import MoveDirection._
      tape(bounded()) = applicableStates.head.nextValue
      state = applicableStates.head.nextState
      head = applicableStates.head.direction match {
        case `left` => head - 1
        case `right` => head + 1
        case `none` => head
        case other => throw new IllegalArgumentException(illegalTypeMessage format (other getClass).getName)
      }
      true
    }
  }

  /**
    * Helper function for filtering available commands by applicability
    *
    * @param command The command to check for applicability with the current execution environment
    * @return true if the command is applicable, false if it isn't
    */
  def isApplicable(command: TuringCommand) = {
    (command.currentValue == tape(bounded()) || (command valueMatchesEverything)) &&
      ((command currentState) == state || (command stateMatchesEveryThing))
  }

  /**
    * Implements [[Array]] wraparound
    *
    * @return [[head]], after bounds correction
    */
  def bounded() = if (head < 0) Math.abs(tapeSize + head) % tapeSize else if (head >= tapeSize) head % tapeSize else head

  def tape = _tape
}

object UniversalTuringMachine {
  val (commentChar, directiveChar, initialStateChar, fillerChar) = (";", "#", "~", "@")
  val illegalTapeSizeMessage = "Illegal Tape Size : "

  /**
    * Allowed characters with special meaning:
    *
    * 1. Lines starting with '[[directiveChar]]' - Directives indicating acceptable halting commands,
    * other than the default halt command
    *
    * 2. Lines starting with '[[commentChar]]' - Discarded as comments.
    * Well, it is a Turing Machine Code program after all, so the assembler style is employed
    *
    * 3. Lines having a '[[commentChar]]' in the middle - Only the part to the left is processed,
    * rest is discarded as an inline comment
    *
    * 4. Line starting with '[[initialStateChar]]' - The initial command indicator
    *
    * 5. Line starting with '[[fillerChar]]' - The tape initializer
    *
    * This [[String]] is split by whitespace after trimming indicator to get filler [[String]]s,
    * which are repeated in order to fill up the tape
    *
    * Without such a line, the tape is initialized to logical blanks, represented by `null`
    *
    * 6. Empty Lines - Ignored and discarded
    *
    * @param data     Raw [[Vector]] of [[String]]s of a Turing Machine Code program
    * @param tapeSize the (fixed) size of the tape. Wraparound is enabled
    * @return An [[UniversalTuringMachine]] object for evaluation of the argument program
    * @see [[directiveChar]]
    * @see [[initialStateChar]]
    * @see [[commentChar]]
    * @see [[fillerChar]]
    * @see [[TuringCommand.whitespaceRegex]]
    */
  def apply(data: Vector[String], tapeSize: Int) = {
    require(tapeSize > 0, s"$illegalTapeSizeMessage$tapeSize")
    def doStartFilter(filterChar: String) = (data filter {
      _ startsWith filterChar
    }).head
    def trim(string: String, trimFrom: String) = string drop (string indexOf trimFrom)
    def process(filterChar: String) = trim(doStartFilter(filterChar), filterChar)
    import TuringCommand._
    val commandsData = data map {
      case normal if !(normal.isEmpty ||
        (normal startsWith commentChar) ||
        (normal startsWith directiveChar) ||
        (normal startsWith initialStateChar)) => TuringCommand(normal)
      case withInlineComment if (withInlineComment indexOf commentChar) > 0 =>
        TuringCommand(trim(withInlineComment, commentChar))
    }
    val initialState = process(initialStateChar)
    val tapeFiller = if (data exists {
      _ startsWith fillerChar
    }) escapeNull((process(fillerChar) split whitespaceRegex) toVector)
    else null
    val terminationData = data map {
      case terminationDirective if terminationDirective startsWith directiveChar => trim(terminationDirective, directiveChar)
    }
    new UniversalTuringMachine(commandsData, initialState, terminationData, tapeFiller, tapeSize)
  }
}