app [main] { pf: platform "platform.roc" }

main : Bool -> [Some Str, None]
main = \return_str ->
    if return_str then
        Some("Hello World!")
    else
        None
