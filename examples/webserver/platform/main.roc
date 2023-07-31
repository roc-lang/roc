platform "webserver"
    requires {} { main : Url -> Task Str [] } # TODO change to U16 for status code
    exposes [
        Path,
        Arg,
        Dir,
        Env,
        File,
        FileMetadata,
        Http,
        Stderr,
        Stdin,
        Stdout,
        Task,
        Tcp,
        Url,
        Utc,
        Sleep,
        Command,
    ]
    packages {}
    imports [Task.{ Task }, Url.{ Url }]
    provides [mainForHost]

mainForHost : Str -> (Task Str [] as Fx)
mainForHost = \str -> main (Url.fromStr str)
# TODO add a hosted effect for getRequest which gets a threadlocal for the current request.
# TODO add a hosted effect for setResponse which sets a threadlocal for the response. Don't expose it in userspace,
# just call it here after everything else is done, so we have the respsone set before we return.
