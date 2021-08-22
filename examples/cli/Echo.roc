#!/usr/bin/env roc

app "echo"
    packages { base: "platform" }
    imports [ base.Task.{ Task }, fx.Effect ]
    provides [ main ] to base

main : Task {} *
main =
    # {} <- await (Stdout.line "What's your first name?")

    # firstName <- await Stdin.line

    # {} <- await (Stdout.line "What's your last name?")

    # lastName <- await Stdin.line
    # Stdout.line "Hi, \(firstName) \(lastName)!"

    # Stdout.line "Hi!"
    Effect.map (Effect.putLine "hi!!!") (\_ -> Ok {})
