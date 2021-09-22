app "webserver"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task }, pf.Route.{ Handler, get0, get1 }, pf.Response, pf.File, pf.UrlParser.{ str } ]
    provides [ runHandlers ] to pf


# imports [ pf.Task.{ Task, await }, pf.Route.{ Handler, get0, get1 }, pf.UrlParser.{ str }, pf.File, pf.Response, pf.Server.{ Server } ]

# server = Server.init { port: 8000, handlers: [] }

runHandlers = \url ->
    Route.runHandlers url
        [ 
            get0 "foo" (Task.succeed (Response.ok "You have reached foo")),
            get0 "bar" (Task.succeed (Response.ok "You have reached bar")),
            get1 "users" str getUser,
            get0 "users" listUsers,
        ]

listUsers : Task Response.Response []
listUsers =
    Task.attempt (File.readUtf8 "users.txt") \result ->
        when result is
            Ok users -> Task.succeed (Response.ok users)
            Err _ -> Task.succeed (Response.status 500 "Could not read users.txt")


getUser : Str -> Task Response.Response []
getUser = \username ->
    response =
        if Str.isEmpty username then
            Response.status 400 "Invalid username `\(username)`"
        else
            Response.ok "Hi, I am the user with this username: \(username)!"

    Task.succeed response
