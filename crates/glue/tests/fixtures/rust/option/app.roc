app [main] { pf: platform "platform.roc" }

main : Bool -> [Some Str, None]
main = \returnStr ->
    if returnStr then
        Some "Hello World!"
    else
        None
