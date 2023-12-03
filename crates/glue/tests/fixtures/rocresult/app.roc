app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main : Bool -> Result Str I32
main = \returnStr ->
    if returnStr then
        Ok "Hello World!"
    else
        Err 42
