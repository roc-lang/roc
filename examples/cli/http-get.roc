app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.9.0/oKWkaruh2zXxin_xfsYsCJobH1tO8_JvNkFzDwwzNUQ.tar.br" }

import pf.Http
import pf.Task exposing [Task]
import pf.Stdin
import pf.Stdout

main : Task {} I32
main =
    _ <- Task.await (Stdout.line "Enter a URL to fetch. It must contain a scheme like \"http://\" or \"https://\".")

    input <- Task.await Stdin.line

    when input is
        End ->
            Stdout.line "I received end-of-input (EOF) instead of a URL."

        Input url ->
            request = {
                method: Get,
                headers: [],
                url,
                mimeType: "",
                body: [],
                timeout: NoTimeout,
            }

            output <- Http.send request
                |> Task.await \resp -> resp |> Http.handleStringResponse |> Task.fromResult
                |> Task.onErr \err -> crash (Http.errorToString err)
                |> Task.await

            Stdout.line output
