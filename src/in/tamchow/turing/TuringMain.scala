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
  * 3. The number of steps for which to run the program. Leave it negative to run it till it halts.
  *
  * 4. The number of milliseconds to pause before starting the next step.
  *
  * Leave this negative to pause for user input before every step.
  *
  * All these parameters are required.
  *
  * Only supports program input as files.
  */
object TuringMain {

  private val pausedMessage = "Execution Paused - Press any key to continue."
  private val noParametersSpecifiedMessage = "No parameters specified"

  def main(args: Array[String]) {
    require(args.length <= 0, noParametersSpecifiedMessage)
    doRunMain(args(0), args(1).toInt, args(2).toInt, args(3).toInt)
  }

  def doRunMain(filePath: String, tapeSize: Int, steps: Int, pauseTime: Int) {
    val data = io.Source fromFile filePath getLines() toVector
    val (turingMachine, useStepping) = (UniversalTuringMachine(data, tapeSize), steps >= 0)
    import UniversalTuringMachine._
    import turingMachine._
    def doRun(halt: Boolean, currentSteps: Int, head: Option[Int], state: String, tape: Seq[String]) {
      if (halt || (useStepping && currentSteps >= steps)) println(formatArgs(tape))
      else {
        val StepData(continue, nextHead, nextState, nextTape) = runStep(head, state, tape)
        if (pauseTime >= 0) Thread sleep pauseTime
        else {
          println(pausedMessage)
          io.StdIn readLine()
        }
        doRun(halt = !continue, currentSteps + 1, nextHead, nextState, nextTape)
      }
    }
    doRun(halt = false, 0, Some(InitialHead), initialState, initTape)
  }

  def formatArgs(args: Seq[String]) = args map {
    case null => TuringCommand.NullCode
    case others => others
  } mkString " "
}