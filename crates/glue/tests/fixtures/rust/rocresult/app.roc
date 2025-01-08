app [main] { pf: platform "platform.roc" }

main : Bool -> Result Str I32
main = \return_str ->
    if return_str then
        Ok("Hello World!")
    else
        Err(42)
