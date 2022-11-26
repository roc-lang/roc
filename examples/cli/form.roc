app "form"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.0/_V6HO2Dwez0xsSstgK8qC6wBLXSfNlVFyUTMg0cYiQQ.tar.br" }
    imports [pf.Stdin, pf.Stdout, pf.Task.{ await, Task }]
    provides [main] to pf

main : Task {} []
main =
    _ <- await (Stdout.line "What's your first name?")
    firstName <- await Stdin.line
    _ <- await (Stdout.line "What's your last name?")
    lastName <- await Stdin.line
    Stdout.line "Hi, \(firstName) \(lastName)! 👋"
