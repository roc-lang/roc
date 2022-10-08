app "helloWorld"
    packages { pf: "cli/cli-platform/main.roc" }
    imports [pf.Stdout, pf.Program.{ Program }]
    provides [main] to pf

main = Program.noArgs mainTask

mainTask =
    Stdout.line "Hello, World!"
    |> Program.exit 0

