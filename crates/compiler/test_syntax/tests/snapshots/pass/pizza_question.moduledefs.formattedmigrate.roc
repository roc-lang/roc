main =
    parse_args? {}
    |> List.drop_first 1
    |> List.map_try? Str.to_u8
    |> List.sum
    |> \total -> "Sum of numbers: $(Num.to_str total)"
    |> Str.to_upper
