package in.tamchow.turing

import scala.language.postfixOps

/**
  * The Launcher for the Universal Turing Machine simulator
  *
  * The parameters are, in order:
  *
  * 1. The input file path
  *
  * 2. The tape size
  *
  * 3. The number of steps for which to run the program.
  * Leave it negative to run it till it halts.
  *
  * 4. The number of milliseconds to pause before starting the next step.
  * Leave this negative to pause for user input before every step.
  *
  * All these parameters are required.
  *
  * Only supports program input as files.
  */
object TuringMain {

  /**
    * The message shown when the execution of the program has been paused
    */
  private val PausedMessage = "Execution Paused - Press any key to continue."
  /**
    * The message shown when no or illegal parameters have been specified
    */
  private val NoOrIllegalParametersSpecifiedMessage = "No or illegal parameters specified : "

  /**
    * The entry point of the application
    *
    * @param args the command-line arguments
    */
  def main(args: Array[String]) {
    try {
      doRunMain(args(0), args(1).toInt, args(2).toInt, args(3).toInt)
    } catch {
      case nfe: NumberFormatException => throw new IllegalArgumentException(s"$NoOrIllegalParametersSpecifiedMessage${args mkString ","}")
      case aiobe: ArrayIndexOutOfBoundsException => throw new IllegalArgumentException(s"$NoOrIllegalParametersSpecifiedMessage${args.length}")
    }
  }

  /**
    * The main runner for the interactive execution of a [[UniversalTuringMachine]]
    *
    * @param filePath  the source file path
    * @param tapeSize  the length of the [[UniversalTuringMachine]]s tape
    * @param steps     the number of steps for which to run the [[UniversalTuringMachine]]
    * @param pauseTime the time to pause (in milliseconds) between steps
    */
  def doRunMain(filePath: String, tapeSize: Int, steps: Int, pauseTime: Int) {
    val data = io.Source.fromFile(filePath).getLines().toSeq
    val (turingMachine, useStepping) = (UniversalTuringMachine(data, tapeSize), steps >= 0)
    import UniversalTuringMachine._
    import turingMachine._
    /**
      * Runs one step of the program
      *
      * @param halt         indicates whether or not halt execution
      * @param currentSteps the current number of steps completed
      * @param head         the head index
      * @param state        the current state
      * @param tape         the current tape
      */
    def doRun(halt: Boolean, currentSteps: Int, head: Option[Int], state: String, tape: Seq[String]) {
      if (halt || (useStepping && currentSteps >= steps)) println(formatSeq(tape))
      else {
        val StepData(continue, nextHead, nextState, nextTape) = runStep(head, state, tape)
        if (pauseTime >= 0) Thread sleep pauseTime
        else {
          println(PausedMessage)
          io.StdIn.readLine()
        }
        doRun(halt = !continue, currentSteps + 1, nextHead, nextState, nextTape)
      }
    }
    doRun(halt = false, 0, Some(InitialHead), initialState, initTape)
  }

  /**
    *
    * @param args the `Seq` to format
    * @return a formatted `String` representation of `args`
    */
  def formatSeq(args: Seq[Any]) = args map {
    case null => TuringCommand.BlankCode
    case others => others
  } mkString " "
}