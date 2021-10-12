#!/usr/bin/env roc

app "echo"
    packages { base: "platform" }
    imports [ base.Task.{ Task, await }, base.Stdout, base.Stdin ]
    provides [ setup ] to base

setup : Task {} *
setup =
    {} <- await (Stdout.line "What's your first name?")

    firstName <- await Stdin.line

    {} <- await (Stdout.line "What's your last name?")

    lastName <- await Stdin.line

    Stdout.line "Hi, \(firstName) \(lastName)!"
