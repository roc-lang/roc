app "webserver"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Route.{ Handler, str, get0, get1 }, pf.Http, pf.File ]
    provides [ server ] to pf

Env :
    {
        # Database connection pool
        dbConns : List Nat
    }

init : Task Env []
init =
    # TODO set up DB connection pool
    Task.succeed { dbConns: [] }

handlers : List (Handler Env)
    [
        get0 "users" listUsers,
        get1 "users" str "" getUser,
    ]

server : Server Env
server =
    { init, handlers }

listUsers : Env -> Task Response []
listUsers = \_ ->
    Task.attempt (File.readUtf8 "users.txt") \result ->
        when result is
            Ok users -> Response.ok users
            Err _ -> Response.status 500 "Could not read users.txt"

getUser : Env, Str -> Task Response []
getUser = \env, username ->
    response =
        if Str.isEmpty username then
            Response.status 400 "Invalid username"
        else
            Response.ok "Hi, I am the user with this username: \(username)!"

    Task.suceced response
