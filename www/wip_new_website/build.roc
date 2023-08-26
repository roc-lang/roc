#!/usr/bin/env roc
app "website-builder"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Task.{ Task },
        pf.Cmd,
    ]
    provides [main] to pf

main =
    # TODO take dist folder name and main.roc path as args once https://github.com/roc-lang/basic-cli/issues/82 is fixed
    # TODO add function to remove boilerplate
    # Remove dist folder
    {} <-
        Cmd.new "rm"
        |> Cmd.args ["-rf", "dist/"]
        |> Cmd.status
        |> Task.onErr \_ -> crash "Failed to remove dist folder"
        |> Task.await

    # Build site
    {} <-
        Cmd.new "roc"
        |> Cmd.args ["run", "main.roc", "--", "content/", "dist/wip/"]
        |> Cmd.status
        |> Task.onErr \_ -> crash "Failed to build site"
        |> Task.await

    # Copy CSS file
    {} <- cp "static/site.css"  "dist/wip/" |> Task.await
    # TODO inline this into repl.js - no need for a separate network request for this.
    {} <- cp "../public/repl/wasi.js"  "dist/wip/" |> Task.await
    # TODO copy this inline into index.html instead of having a separate network request for this tiny file!
    {} <- cp "../public/repl/repl.js"  "dist/wip/" |> Task.await

    {} <- cp "static/roc-rocket.png"  "dist/wip/" |> Task.await

    # Copy font files - assume that www/build.sh has been run previously and the
    # fonts are available locally in ../build/fonts
    {} <-
        Cmd.new "cp"
        |> Cmd.args ["-r", "../build/fonts/", "dist/fonts/"]
        |> Cmd.status
        |> Task.onErr \_ -> crash "Failed to copy static files"
        |> Task.await

    # Start file server
    {} <-
        Cmd.new "simple-http-server"
        |> Cmd.args ["-p", "8080", "--nocache", "--index", "--", "dist/"]
        |> Cmd.status
        |> Task.onErr \_ -> crash "Failed to run file server; consider intalling with `cargo install simple-http-server`"
        |> Task.await

    Task.ok {}

cp = \src, dest ->
    Cmd.new "cp"
    |> Cmd.args ["-r", src, dest]
    |> Cmd.status
    |> Task.onErr \_ -> crash "Failed to cp from \(src) to \(dest)"
