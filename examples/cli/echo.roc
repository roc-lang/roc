app "echo"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdin, pf.Stdout, pf.Task.{ Task }, pf.Program.{ Program, ExitCode }]
    provides [main] to pf

main : Program
main = Program.noArgs mainTask

mainTask : Task ExitCode [] [Write [Stdout]]
mainTask =
    (Stdout.line "🗣  Shout into this cave and hear the echo! 👂👂👂")
    |> Program.exit 0
