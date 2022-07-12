app "main"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main =
  # input = "0" # <- Leaks 54 bytes of memory
  # input = "0123456789ABCDEFGHIJKLM" # <- Leaks 76 bytes of memory
  input = "0123456789ABCDEFGHIJKLMN" # <- Use after free
  when input |> Str.toUtf8 |> manyImpl anyChar []  is
    Ok result ->
      val = result.val |> strFromRaw
      leftover = result.input |> strFromRaw
      "Success: \(val) (leftover string: \(leftover))\n"
    Err (ParsingFailure problem) ->
      "Failure: \(problem)\n"

manyImpl = \input, parser, vals ->
  result = (parser input)
  when result is
    Err _ ->
      Ok {val: vals, input: input}
    Ok {val: val, input: inputRest} ->
      manyImpl inputRest parser (List.append vals val)

anyChar =
  \input ->
    {before: start, others: inputRest} = List.split input 1
    when List.get start 0 is
      Err OutOfBounds ->
        Err (ParsingFailure "expected any char, but input was empty.")
      Ok startCodepoint ->
        Ok {val: startCodepoint, input: inputRest}

strFromRaw : List U8 -> Str
strFromRaw = \rawStr ->
  rawStr
  |> Str.fromUtf8
  |> Result.withDefault "Unexpected problem while turning a List U8 (that was originally a Str) back into a Str. This should never happen!"
