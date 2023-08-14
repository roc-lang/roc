#!/usr/bin/env roc
app "website-builder"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Task.{ Task },
        pf.Command,
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
        |> Task.onFail \_ -> crash "Failed to remove dist folder"
        |> Task.await

    # Build site
    {} <- 
        Command.new "roc"
        |> Command.args ["run", "main.roc", "--", "content/", "dist/wip/"]
        |> Command.status
        |> Task.onFail \_ -> crash "Failed to build site"
        |> Task.await

    # Copy static files
    {} <- 
        Command.new "cp"
        |> Command.args ["-r", "static/site.css", "dist/wip/"]
        |> Command.status
        |> Task.onFail \_ -> crash "Failed to copy static files"
        |> Task.await

    # Copy font files - assume that www/build.sh has been run previously and the
    # fonts are available locally in ../build/fonts
    {} <- 
        Command.new "cp"
        |> Command.args ["-r", "../build/fonts/", "dist/fonts/"]
        |> Command.status
        |> Task.onFail \_ -> crash "Failed to copy static files"
        |> Task.await

    # Start file server
    {} <- 
        Command.new "simple-http-server"
        |> Command.args ["-p", "8080", "--nocache", "--index", "--", "dist/"]
        |> Command.status
        |> Task.onFail \_ -> crash "Failed to run file server; consider intalling with `cargo install simple-http-server`"
        |> Task.await

    Task.succeed {}
