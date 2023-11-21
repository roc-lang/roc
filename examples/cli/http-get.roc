app "http-get"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.0/QOQW08n38nHHrVVkJNiPIjzjvbR3iMjXeFY5w1aT46w.tar.br" }
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
                body: Http.emptyBody,
                timeout: NoTimeout,
            }

            output <- Http.send request
                |> Task.onErr \err -> err
                    |> Http.errorToString
                    |> Task.ok
                |> Task.await

            Stdout.line output
