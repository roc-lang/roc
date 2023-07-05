#!/usr/bin/env roc
app "website-builder"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.4.0-rc1/hbIodFf7kULTYZJkzgsvgsnFAvQexm5hVeBaOMZk84I.tar.br" }
    imports [
        pf.Task.{ Task },
        pf.Command,
        pf.Arg
    ]
    provides [main] to pf

main = 
    # TODO take dist folder name and main.roc path as args once https://github.com/roc-lang/basic-cli/issues/82 is fixed
    # TODO add function to remove boilerplate
    # Remove dist folder
    {} <- 
        Command.new "rm"
        |> Command.args ["-rf", "dist/"]
        |> Command.status
        |> Task.onErr \_ -> crash "Failed to remove dist folder"
        |> Task.await

    # Build site
    {} <- 
        Command.new "roc"
        |> Command.args ["run", "main.roc", "--", "content/", "dist/"]
        |> Command.status
        |> Task.onErr \_ -> crash "Failed to build site"
        |> Task.await

    # Copy static files
    {} <- 
        Command.new "cp"
        |> Command.args ["-r", "static/site.css", "dist/"]
        |> Command.status
        |> Task.onErr \_ -> crash "Failed to copy static files"
        |> Task.await

    # Start file server
    {} <- 
        Command.new "simple-http-server"
        |> Command.args ["-p", "8080", "--", "dist/"]
        |> Command.status
        |> Task.onErr \_ -> crash "Failed to run file server; consider intalling with `cargo install simple-http-server`"
        |> Task.await

    Task.ok {}
