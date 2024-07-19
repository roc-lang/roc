app [main] { pf: platform "platform.roc" }

main : Bool -> Result Str I32
main = \returnStr ->
    if returnStr then
        Ok "Hello World!"
    else
        Err 42
