#!/usr/bin/env roc
app "website-builder"
    # TODO update to basic-cli release when Command module is available 
    packages { pf: "https://github.com/lukewilliamboswell/roc-things/releases/download/test/VQYlmhbzLld4kAE4Y8sWt349md89iyGBg5hgoIBfvcs.tar.br" }
    imports [
        pf.Task.{ Task },
        pf.Command,
    ]
    provides [main] to pf

main = 

    # Remove dist folder
    {} <- 
        Command.new "rm"
        |> Command.arg "-rf"
        |> Command.arg "dist/"
        |> Command.status
        |> Task.onFail \_ -> crash "Failed to remove dist folder"
        |> Task.await

    # Build site
    {} <- 
        Command.new "roc"
        |> Command.arg "run"
        |> Command.arg "main.roc"
        |> Command.arg "--"
        |> Command.arg "content/"
        |> Command.arg "dist/"
        |> Command.status
        |> Task.onFail \_ -> crash "Failed to build site"
        |> Task.await

    # Copy static files
    {} <- 
        Command.new "cp"
        |> Command.arg "-r"
        |> Command.arg "static/site.css"
        |> Command.arg "dist/"
        |> Command.status
        |> Task.onFail \_ -> crash "Failed to copy static files"
        |> Task.await

    # Start file server
    {} <- 
        Command.new "simple-http-server"
        |> Command.arg "-p"
        |> Command.arg "8080"
        |> Command.arg "--"
        |> Command.arg "dist/"
        |> Command.status
        |> Task.onFail \_ -> crash "Failed to run file server; consider intalling with `cargo install simple-http-server`"
        |> Task.await

    Task.succeed {}
