main =
    parseArgs? {}
    |> List.dropFirst 1
    |> List.mapTry? Str.toU8
    |> List.sum
    |> \total -> "Sum of numbers: $(Num.to_str total)"
    |> Str.toUpper
