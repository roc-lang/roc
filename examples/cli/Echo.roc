#!/usr/bin/env roc run

app "echo"
    packages { base: "platform" }
    imports [ base.Task.{ await }, base.Stdout, base.Stdin ]
    provides [ main ] to base

main : Task.Task {} *
main =
    {} <- await (Stdout.line "What's your first name?")

    firstName <- await Stdin.line

    {} <- await (Stdout.line "What's your last name?")

    lastName <- await Stdin.line

    Stdout.line "Hi, \(firstName) \(lastName)!"
