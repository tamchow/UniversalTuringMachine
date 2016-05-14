package in.tamchow.turing

/**
  * The Launcher for the Universal Turing Machine simulator
  *
  * The parameters are, in order:
  *
  * 1. The input file path
  * 2. The tape size
  * 3. The number of steps for which to run the program. Leave it negative to run it till it halts.
  * 4. The number of milliseconds to pause before starting the next step. Leave this negative to pause for user input before every step.
  *
  * All these parameters are required.
  *
  * Only supports input as files.
  */
object TuringMain {

  val pausedMessage = "Execution Paused - Press any key to continue."

  def doRun(filePath: String, tapeSize: Int, steps: Int, pauseTime: Int): Unit = {
    val data = io.Source fromFile filePath getLines() toList
    val turingMachine = UniversalTuringMachine fromStrings(data, tapeSize)
    val useStepping = steps >= 0
    var currentSteps = 0
    var halt = false
    import util.control.Breaks._
    breakable {
      while (!halt) {
        if (useStepping && currentSteps >= steps) break
        println(turingMachine tape)
        halt = turingMachine runStep()
        currentSteps += 1
        if (pauseTime >= 0) Thread sleep pauseTime
        else {
          println(pausedMessage)
          System.in.read()
        }
      }
    }
  }

  def main(args: Array[String]) {
    if (args.length <= 0) throw new IllegalArgumentException("No parameters specified")
    val filePath = args(0)
    val tapeSize = args(1).toInt
    val steps = args(2).toInt
    val pauseTime = args(3).toInt
    doRun(filePath, tapeSize, steps, pauseTime)
  }
}