app "echo"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Stdout, pf.Stdin ]
    provides [ main ] to pf

main : Task {} *
main =
    {  } <- await (Stdout.line "What's your first name?")

    firstName <- await Stdin.line

    {  } <- await (Stdout.line "What's your last name?")

    lastName <- await Stdin.line

    Stdout.line "Hi, \(firstName) \(lastName)!"
