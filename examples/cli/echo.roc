app "echo"
    packages { pf: "platform" }
    imports [ pf.Stdin, pf.Stdout, pf.Task ]
    provides [ main ] to pf

main : Task.Task {} *
main = 
    _ <- Task.await (Stdout.line "Shout into this cave and hear the echo!")
    Task.forever tick

tick =
    shout <- Task.await Stdin.line
    Stdout.line shout
