app "http-get"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Http, pf.Task, pf.Stdin, pf.Stdout, pf.Program.{ Program, ExitCode }]
    provides [main] to pf

main : Program
main = Program.noArgs mainTask

mainTask : Task.Task ExitCode [] [Read [Stdin], Write [Stdout], Network [Http]]
mainTask =
    _ <- Task.await (Stdout.line "Please enter a URL to fetch")

    url <- Task.await Stdin.line

    request = {
        method: Get,
        headers: [],
        url,
        body: Http.emptyBody,
        timeout: NoTimeout,
    }

    output <- Http.send request
        |> Task.onFail (\err -> err |> Http.errorToString |> Task.succeed)
        |> Task.await

    Stdout.line output
    |> Program.exit 0
