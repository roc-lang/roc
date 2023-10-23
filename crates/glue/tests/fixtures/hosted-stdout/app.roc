app "app"
    packages { pf: "platform.roc" }
    imports [pf.Task]
    provides [main] to pf

main : Task.Task {} []
main = Task.stdoutLine "hello world" 
