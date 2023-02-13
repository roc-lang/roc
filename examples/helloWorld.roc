app "helloWorld"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.2.0/8tCohJeXMBUnjo_zdMq0jSaqdYoCWJkWazBd4wa8cQU.tar.br" }
    imports [pf.Stdout, pf.Stdin, pf.Task]
    provides [main] to pf

main = test

test =
    a <- Task.await Stdin.line
    if a == "done" then Task.succeed {} else test
