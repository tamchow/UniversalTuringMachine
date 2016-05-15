package in.tamchow.turing

import scala.language.postfixOps

/**
  * Represents a universal Turing Machine
  */
class UniversalTuringMachine(commands: List[TuringCommand], initialState: String, terminalStates: List[String], tapeSize: Int) {
  private val _tape = new Array[String](tapeSize)
  private var (head, state) = (0, initialState)

  /**
    * Runs the program for the specified number of steps
    * or till the program halts (if the specified number of steps is negative)
    *
    * @param steps the number of steps for which the program is run.
    *              Supply negative values to run till halt
    * @return the history of the tape in each step as a [[List]] of [[Array]]s of [[String]]s
    */
  def runForStepsOrTillHalt(steps: Int) = {
    var currentSteps = 0
    var halt = initialState == null
    val useStepping = steps >= 0
    var tapeHistory: List[Array[String]] = Nil
    while (!(halt || (useStepping && currentSteps >= steps))) {
      tapeHistory = tapeHistory :+ tape
      halt = runStep()
      currentSteps += 1
    }
    tapeHistory
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
      val applicableState = applicableStates.head
      tape(bounded()) = applicableState.nextValue
      state = applicableState.nextState
      head = applicableState.direction match {
        case LEFT => head - 1
        case RIGHT => head + 1
        case NONE => head
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
      ((command currentState) == this.state || (command stateMatchesEveryThing))
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
  val (commentChar, directiveChar, initialStateChar) = (";", "#", "~")

  /**
    * Allowed characters with special meaning:
    *
    * 1. Lines starting with '#' - Directives indicating acceptable halting commands, other than the default halt command
    * 2. Lines starting with ';' - Discarded as comments.
    * Well, it is a Turing Machine Code program after all, so the assembler style is employed
    * 3. Lines having a ';' in the middle - Only the part to the left is processed, rest is discarded as an inline comment
    * 4. Lines starting with '~' - The initial command indicator
    * 5. Empty Lines - Ignored and discarded
    *
    * @param data     Raw [[List]] of [[String]]s of a Turing Machine Code program
    * @param tapeSize the (fixed) size of the tape. Wraparound is enabled
    * @return An [[UniversalTuringMachine]] object for evaluation of the argument program
    * @see [[UniversalTuringMachine.directiveChar]]
    * @see [[UniversalTuringMachine.initialStateChar]]
    * @see [[UniversalTuringMachine.commentChar]]
    */
  def apply(data: List[String], tapeSize: Int) = {
    val commandsData = data map {
      case normal if !(normal.isEmpty ||
        (normal startsWith commentChar) ||
        (normal startsWith directiveChar) ||
        (normal startsWith initialStateChar)) => TuringCommand(normal)
      case withInlineComment if (withInlineComment indexOf commentChar) > 0 =>
        TuringCommand(withInlineComment substring(0, withInlineComment indexOf commentChar))
    }
    var initialState = (data filter {
      _ startsWith initialStateChar
    }).head
    initialState = initialState substring(initialState indexOf initialStateChar, initialState length)
    val terminationData = data map {
      case terminationDirective if terminationDirective startsWith directiveChar =>
        terminationDirective substring(terminationDirective indexOf directiveChar, terminationDirective length)
    }
    new UniversalTuringMachine(commandsData, initialState, terminationData, tapeSize)
  }
}