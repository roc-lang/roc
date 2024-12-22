foo
|> Dict.keep_if \(k, _v) -> List.contains keys_to_delete k |> Bool.not