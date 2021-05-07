#!/usr/bin/env roc run

app "philly-ete-demo"
    packages { platform: "sandboxed-cli" }

    # There's code below all these comments...but we can't see what it's doing!
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    #
    imports [ platform.Task.{ Task, await }, platform.Stdout, platform.Http ]
    provides [ main ] to platform

main =
    {} <- await (Stdout.line "Doing totally trustworthy things, nothing to worry about...")

    result <- Task.attempt (Http.getUtf8 "https://malicious.example.com")

    when result is
        Ok _ -> Task.succeed {}
        Err _ -> Task.succeed {}
