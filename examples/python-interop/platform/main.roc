platform "python-interop"
    requires {} { main : U64 -> Str }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

main_for_host : List U8 -> List U8
main_for_host = \input ->
    when Str.from_utf8(input) is
        Ok(arg) ->
            when Str.to_u64(arg) is
                Ok(num) -> main(num) |> Str.to_utf8
                Err(_) -> []

        Err(_) -> []
