package in.tamchow.turing

import in.tamchow.turing.MoveDirection._

import scala.language.postfixOps

/**
  * Represents a universal Turing Machine
  */
class UniversalTuringMachine(states: List[TuringState], initialState: String, terminalStates: List[String], tapeSize: Int) {
  val tape: Array[String] = Array.fill(tapeSize) {
    null
  }
  var head = 0
  var state = initialState

  /**
    * Runs the program for the specified number of steps
    * or till the program halts (if the specified number of steps is negative)
    *
    * @param steps the number of steps for which the program is run.
    *              Supply negative values to run till halt
    * @return the history of the tape in each step as a [[List]] of [[Array]]s of [[String]]s
    */
  def runForStepsOrTillHalt(steps: Int): List[Array[String]] = {
    var currentSteps = 0
    var halt = initialState == null
    val useStepping = steps >= 0
    var tapeHistory: List[Array[String]] = Nil
    import util.control.Breaks._
    breakable {
      while (!halt) {
        if (useStepping && currentSteps >= steps) break
        tapeHistory = tapeHistory :+ tape
        halt = runStep()
        currentSteps += 1
      }
    }
    tapeHistory
  }

  /**
    * Runs one step of a program
    *
    * @return true if there are more steps possible, else false
    */
  def runStep(): Boolean = {
    val applicableStates = states filter (isApplicable(_))
    if (applicableStates.isEmpty || state == null || (terminalStates contains state)) false
    else {
      val applicableState = applicableStates.head
      tape(bounded()) = applicableState.nextValue
      state = applicableState.nextState
      head = applicableState.direction match {
        case LEFT => head - 1
        case RIGHT => head + 1
        case _ => head
      }
      true
    }
  }

  /**
    * Implements [[Array]] wraparound
    *
    * @return [[head]], after bounds correction
    */
  def bounded(): Int = {
    if (head < 0) Math.abs(tapeSize + head) % tapeSize
    else if (head >= tapeSize) head % tapeSize
    else head
  }

  /**
    * Helper function for filtering available commands by applicability
    *
    * @param state The state to check for applicability with the current execution environment
    * @return true if the state is applicable, false if it isn't
    */
  //Stupid Idea - Even does this for Java sometimes
  //noinspection ComparingUnrelatedTypes
  def isApplicable(state: TuringState): Boolean = {
    (state.currentValue == tape(bounded()) || (state valueMatchesEverything())) &&
      (state.currentState == this.state || (state stateMatchesEveryThing()))
  }
}

object UniversalTuringMachine {
  val COMMENT_CHAR = ";"
  val DIRECTIVE_CHAR = "#"
  val INITIAL_STATE_CHAR = "~"

  /**
    * Allowed characters with special meaning:
    *
    * 1. Lines starting with '#' - Directives indicating acceptable halting states, other than the default halt state
    * 2. Lines starting with ';' - Discarded as comments.
    * Well, it is a Turing Machine Code program after all, so the assembler style is employed
    * 3. Lines having a ';' in the middle - Only the part to the left is processed, rest is discarded as an inline comment
    * 4. Lines starting with '~' - The initial state indicator
    * 5. Empty Lines - Ignored
    *
    * @param data     Raw [[List]] of [[String]]s of a Turing Machine Code program
    * @param tapeSize the (fixed) size of the tape. Wraparound is enabled
    * @return An [[UniversalTuringMachine]] object for evaluation of the argument program
    * @see [[UniversalTuringMachine.DIRECTIVE_CHAR]]
    * @see [[UniversalTuringMachine.INITIAL_STATE_CHAR]]
    * @see [[UniversalTuringMachine.COMMENT_CHAR]]
    */
  def fromStrings(data: List[String], tapeSize: Int): UniversalTuringMachine = {
    val commandsData = data map {
      case normal if !(normal.isEmpty ||
        (normal startsWith COMMENT_CHAR) ||
        (normal startsWith DIRECTIVE_CHAR) ||
        (normal startsWith INITIAL_STATE_CHAR)) => TuringState fromString normal
      case withInlineComment if (withInlineComment indexOf COMMENT_CHAR) > 0 =>
        TuringState fromString (withInlineComment substring(0, withInlineComment indexOf COMMENT_CHAR))
    }
    var initialState = (data filter {
      _ startsWith "~"
    }).head
    initialState = initialState substring(initialState indexOf INITIAL_STATE_CHAR, initialState length)
    val terminationData = data map {
      case terminationDirective if terminationDirective startsWith DIRECTIVE_CHAR =>
        terminationDirective substring(terminationDirective indexOf DIRECTIVE_CHAR, terminationDirective length)
    }
    new UniversalTuringMachine(commandsData, initialState, terminationData, tapeSize)
  }
}