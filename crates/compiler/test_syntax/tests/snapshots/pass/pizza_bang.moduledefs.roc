main =
    Arg.list! {}
    |> List.dropFirst 1
    |> List.mapTry Str.toU8
    |> Task.fromResult!
    |> List.sum
    |> \total -> "Sum of numbers: $(Num.toStr total)"
    |> Stdout.line!
