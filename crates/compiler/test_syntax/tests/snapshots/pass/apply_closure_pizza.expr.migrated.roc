foo
|> Dict.keepIf(|(k,_v,)| List.contains(keysToDelete,k,) |> Bool.not,)
