app "helloWorld"
    packages { pf: "interactive/cli-platform/main.roc" }
    imports [pf.Stdout]
    provides [main] to pf

main = Stdout.line "Hello, World!"
