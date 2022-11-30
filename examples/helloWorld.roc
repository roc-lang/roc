app "helloWorld"
    packages { pf: "cli/cli-platform/main.roc" }
    imports [pf.Stdout]
    provides [main] to pf

main =
    Stdout.line "Hello, World!"
