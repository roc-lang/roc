app "form"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdin, pf.Stdout, pf.Task.{ await, Task }, pf.Program.{ Program, ExitCode }]
    provides [main] to pf

main : Program
main = Program.noArgs mainTask

mainTask : Task ExitCode [] [Read [Stdin], Write [Stdout]]
mainTask =
    _ <- await (Stdout.line "What's your first name?")
    firstName <- await Stdin.line
    _ <- await (Stdout.line "What's your last name?")
    lastName <- await Stdin.line
    Stdout.line "Hi, \(firstName) \(lastName)! 👋"
    |> Program.exit 0
