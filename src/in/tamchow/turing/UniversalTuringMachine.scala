package in.tamchow.turing

import scala.language.postfixOps

/**
  * Represents a universal Turing Machine
  */
class UniversalTuringMachine(val commands: Vector[TuringCommand], val initialState: String, val terminalStates: Vector[String], val tapeFiller: Vector[String], val tapeSize: Int) {
  /**
    * Runs the program for the specified number of steps
    * or till the program halts (if the specified number of steps is negative)
    *
    * @param steps the number of steps for which the program is run.
    *              Supply negative values to run till halt
    * @return the history of the tape in each step as a [[scala.Vector]] of [[scala.Vector]]s of [[java.lang.String]]s
    */
  def runForStepsOrTillHalt(steps: Int) = {
    val useStepping = steps >= 0
    def doRun(halt: Boolean, currentSteps: Int, head: Int, state: String, tape: Vector[String], tapeHistory: Vector[Vector[String]]): Vector[Vector[String]] = {
      if (halt || (useStepping && currentSteps >= steps)) tapeHistory
      else {
        val (continue, nextHead, nextState, nextTape) = runStep(head, state, tape)
        doRun(!continue, currentSteps + 1, nextHead, nextState, nextTape, tapeHistory :+ tape)
      }
    }
    doRun(halt = false, 0, 0, initialState, initTape, Vector())
  }

  /**
    * Fills the tape initially with the [[tapeFiller]] contents repeatedly.
    *
    * @return a properly initialized tape
    */
  def initTape: Vector[String] = {
    if (tapeFiller == null) (Vector fill tapeSize) (null)
    else {
      val (times, remainder) = (tapeSize / tapeFiller.length, tapeSize % tapeFiller.length)
      def fill(fillCount: Int, struct: Vector[String]): Vector[String] = {
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
  def runStep(head: Int, state: String, tape: Vector[String]) = {
    val applicableStates = commands filter (isApplicable(_, head, state, tape))
    if (applicableStates.isEmpty || state == null || (terminalStates contains state)) (false, -1, null, null)
    else {
      (true, applicableStates.head.direction match {
        case MoveDirection.left => head - 1
        case MoveDirection.right => head + 1
        case MoveDirection.none => head
        case other => throw new IllegalArgumentException(MoveDirection.illegalTypeMessage format (other getClass).getName)
      }, applicableStates.head.nextState, tape.updated(bounded(head), applicableStates.head.nextValue))
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
  private[this] def isApplicable(command: TuringCommand, head: Int, state: String, tape: Vector[String]) = {
    (command.currentValue == tape(bounded(head)) || (command valueMatchesEverything)) &&
      ((command currentState) == state || (command stateMatchesEveryThing))
  }

  /**
    * Implements [[Array]] wraparound
    *
    * @param head the argument to bounds-correct
    * @return the argument after bounds correction
    */
  private[this] def bounded(head: Int) =
    if (head < 0) Math.abs(tapeSize + head) % tapeSize else if (head >= tapeSize) head % tapeSize else head
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
  def apply(data: Vector[String], tapeSize: Int) = {
    require(tapeSize > 0, s"$illegalTapeSizeMessage$tapeSize")
    def doStartFilter(filterChar: String) = (data filter {
      _ startsWith filterChar
    }).head
    def trim(string: String, trimFrom: String) = string drop (string indexOf trimFrom)
    def process(filterChar: String) = trim(doStartFilter(filterChar), filterChar)
    def qualifier(item: String, char: String) = item startsWith char
    def isCommand(item: String) =
      !(item.isEmpty || qualifier(item, commentChar) || qualifier(item, directiveChar) || qualifier(item, initialStateChar))
    def hasMidLineComment(item: String) = (item indexOf commentChar) > 0
    import TuringCommand._
    val commandsData = for (item <- data if isCommand(item) || hasMidLineComment(item)) yield
      if (isCommand(item)) TuringCommand(item) else if (hasMidLineComment(item)) TuringCommand(trim(item, commentChar))
    val initialState = process(initialStateChar)
    val tapeFiller = if (data exists (qualifier(_, fillerChar)))
      escapeNull((process(fillerChar) split whitespaceRegex) toVector)
    else null
    val terminationData = for (terminationDirective <- data if qualifier(terminationDirective, directiveChar)) yield
      trim(terminationDirective, directiveChar)
    new UniversalTuringMachine(commandsData.asInstanceOf[Vector[TuringCommand]], initialState, terminationData, tapeFiller, tapeSize)
  }
}