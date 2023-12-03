app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main : Bool -> [ Some Str, None ] 
main = \returnStr ->
    if returnStr then
        Some "Hello World!"
    else
        None
