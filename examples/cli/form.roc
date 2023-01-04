app "form"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdin, pf.Stdout, pf.Task.{ await, Task }]
    provides [main] to pf

main : Task {} []
main =
    _ <- await (Stdout.line "What's your first name?")
    firstName <- await Stdin.line
    _ <- await (Stdout.line "What's your last name?")
    lastName <- await Stdin.line
    Stdout.line "Hi, \(firstName) \(lastName)! 👋"
