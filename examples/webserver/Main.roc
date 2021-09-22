app "webserver"
    packages { pf: "platform" }
    imports [ pf.Task.{ Task, await }, pf.Route.{ Handler, get0, get1 }, pf.UrlParser.{ str }, pf.File, pf.Response, pf.Server.{ Server } ]
    provides [ server ] to pf


server : Server
server =
    Server.init { port: 8000, handlers }


handlers: List Handler
handlers =
    []
        # get0 "users" listUsers,
        # get1 "users" str "" getUser,


# listUsers : Task Response []
# listUsers =
#     Task.attempt (File.readUtf8 "users.txt") \result ->
#         when result is
#             Ok users -> Response.ok users
#             Err _ -> Response.status 500 "Could not read users.txt"


# getUser : Str -> Task Response []
# getUser = \username ->
#     response =
#         if Str.isEmpty username then
#             Response.status 400 "Invalid username"
#         else
#             Response.ok "Hi, I am the user with this username: \(username)!"

#     Task.suceced response
