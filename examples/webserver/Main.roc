app "webserver"
    packages { base: "platform" }
    imports []
    provides [ main ] to base

main: Str -> Str
main = \url ->
    segments = Str.split url "/"
    when List.get segments 1 is
        Err _ ->
            "Hello, World!\nWelcome to the home page."
        Ok val ->
            laterSegments = List.drop segments 2
            if val == "roc" then
                "Wow, you found the secret page!"
            else if val == "name" then
                handleName laterSegments
            else
                "Hello, World to all the people at \(val)!"

handleName: List Str -> Str
handleName = \segments ->
    if List.len segments == 0 then
        "Hello, no name!"
    else
        name = Str.joinWith segments " "
        "Hello, \(name)!"