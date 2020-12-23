app "echo"
    packages { base: "platform/" }
    imports [ base.Task.{ after }, base.Stdout, base.Stdin ]
    provides [ main ] to base

main : Task.Task {} *
main =
    after (Stdout.line "What's your first name?") \{} ->
    after Stdin.line \firstName ->
    after (Stdout.line "What's your last name?") \{} ->
    after Stdin.line \lastName ->
        Stdout.line "Hi, \(firstName) \(lastName)!"
