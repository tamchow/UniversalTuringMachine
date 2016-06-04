So after reading Programming in Scala and a few other books, I tried my hand at making a Universal Turing Machine Simulator. I tried my best to make it as idiomatic, functional (as in the *paradigm*, not *usability*) and simple(?) as possible.

__Notes about formatting and scope of review__:

This was run through the Idea 2016.1 formatter for Scala set on default.

The `TuringMain` object, is, as usual, not really up for review, however, any logic or organization comments will be appreciated.

__Usage__:

The project is hosted [here](https://github.com/tamchow/UniversalTuringMachine), grab the latest release from there to run it. The documentation is hosted [here](http://tamchow.github.io/UniversalTuringMachine). Basically, this code really overuses `Vector`s instead of `List`s, so it requires Scala 2.8 and newer. I develop on Scala 2.11.8 with IntelliJ Idea 2016.1.2 and JDK 1.8_u40. The release `.jar` has the Scala runtime bundled into it so it can be run normally like any other JAR file.

For lack of time, the launcher takes positional parameters rather than named ones. [This](http://tamchow.github.io/UniversalTuringMachine/#in.tamchow.turing.TuringMain$) describes the parameters and their positions, but it basically goes as:

1. The input file path

2. The tape size

3. The number of steps for which to run the program. Leave it negative to run it till it halts.

4. The number of milliseconds to pause before starting the next step. Leave this negative to pause for user input before every step.

The Turing Machine code files are simple text files, no extension is enforced.

There are some special characters usable in these files, and they are as follows:

Allowed characters with special meaning:

1. Lines starting with `DirectiveChar` (`#`) - Directives indicating acceptable halting commands, other than the default halt command

2. Lines starting with `CommentChar` (`;`) - Discarded as comments. Well, it is a Turing Machine Code program after all, so the assembler style is employed

3. Lines having a `CommentChar` (`;`) in the middle - Only the part to the Left is processed, rest is discarded as an inline comment

4. Line starting with `InitialStateChar` (`~`) - The initial command indicator

5. Line starting with `FillerChar` (`@`) - The tape initializer. This String is split by whitespace after trimming the indicator character to get filler Strings, which are repeated in order to fill up the tape. Without such a line, the tape is initialized to logical blanks, represented by null

6. Empty Lines - Ignored and discarded

A particular operation for the Turing Machine is denoted as follows:

`<current state> <next state> <current value> <next value> <direction>`

Two wildcards are allowed for anything except the direction of tape head movement:

1. `!` - indicates a null value or the halting command, internally represented as `null`.

2. `*` - indicates a match-all wildcard character, no separate internal representation

`<direction>` indicates the direction of tape head movement.

Left by 1 cell is indicated by anything starting with `"L"` or `"l"` or a negative integer.

Right by 1 cell is indicated by anything starting with `"R"` or `"r"` or a positive integer.

Neither of the above is no movement.

Hence follows the code: