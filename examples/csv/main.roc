app "main"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main = partialTest deconstructedParser "0123456789ABCDEFGHIJKLMN"


RawStr : List U8

partialTest = \parser, input ->
  when input |> strToRaw |> parser  is
    Ok result ->
      # val = result.val |> Str.joinWith("\r\n")
      val = result.val
      leftover = result.input
      "Parse success: \(val) (leftover string: \(leftover))\n"
    Err (ParsingFailure problem) ->
      "Parse failure: \(problem)\n"

deconstructedParser =
  \input ->
    manyImpl (anyChar) [] input
    |> Result.map \field ->
      res = field.val |> strFromRaw
      {val: res, input: field.input |> strFromRaw }

manyImpl = \parser, vals, input ->
  result = (parser input)
  when result is
    Err _ ->
      Ok {val: vals, input: input}
    Ok {val: val, input: inputRest} ->
      manyImpl parser (List.append vals val) inputRest

anyChar =
  \input ->
    {before: start, others: inputRest} = List.split input 1
    when List.get start 0 is
      Err OutOfBounds ->
        Err (ParsingFailure "expected any char, but input was empty.")
      Ok startCodepoint ->
        Ok {val: startCodepoint, input: inputRest}

strToRaw : Str -> RawStr
strToRaw = \str ->
  str |> Str.toUtf8

strFromRaw : RawStr -> Str
strFromRaw = \rawStr ->
  rawStr
  |> Str.fromUtf8
  |> Result.withDefault "Unexpected problem while turning a List U8 (that was originally a Str) back into a Str. This should never happen!"
