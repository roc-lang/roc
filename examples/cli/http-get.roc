app "http-get"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.9.1/y_Ww7a2_ZGjp0ZTt9Y_pNdSqqMRdMLzHMKfdN8LWidk.tar.br" }
    imports [pf.Http, pf.Task.{ Task }, pf.Stdin, pf.Stdout]
    provides [main] to pf

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
