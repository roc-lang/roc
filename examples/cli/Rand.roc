#!/usr/bin/env roc

app "rand"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Stdout, pf.Rand ]
    provides [ main ] to pf

main : Task {} *
main =
    nat <- await Rand.nat

    natStr = Str.fromInt nat

    Stdout.line "Here's a random number: \(natStr)"
