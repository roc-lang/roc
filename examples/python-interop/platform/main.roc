platform "python-interop"
    requires {} { main : U64 -> Str }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : List U8 -> List U8
mainForHost = \input ->
    when Str.fromUtf8 input is
        Ok arg ->
            when Str.toU64 arg is
                Ok num -> main num |> Str.toUtf8
                Err _ -> []

        Err _ -> []
