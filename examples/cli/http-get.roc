app "http-get"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [pf.Http, pf.Task.{ Task }, pf.Stdin, pf.Stdout]
    provides [main] to pf

main : Task {} I32
main =
    _ <- Stdout.line "Please enter a URL to fetch" |> Task.await

    urlInput <- Stdin.line |> Task.await

    when urlInput is
        End -> Task.ok {}
        Input url ->
            request = {
                method: Get,
                headers: [],
                url,
                body: Http.emptyBody,
                timeout: NoTimeout,
            }

            output <- Http.send request
                |> Task.onErr \err -> Task.ok (Http.errorToString err)
                |> Task.await

            Stdout.line output
