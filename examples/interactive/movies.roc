app "movies"
    packages { pf: "cli-platform/main.roc" }
    imports []
    provides [main] to pf

main = Task.succeed {}

expect 1 == 1
