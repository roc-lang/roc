platform "cli"
    requires {} { main : Task {} [] }
    exposes []
    packages {}
    imports [Task.{ Task }]
    provides [mainForHost]

mainForHost : Task (List U8) []
mainForHost = 
    main 
    |> Task.map responseToBytes

responseToBytes : Response -> List U8
responseToBytes = \{ status, headers, body } ->
    crash "kaboom!"
