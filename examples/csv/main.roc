app "main"
    packages { pf: "platform/main.roc" }
    imports [Parser.Core.{Parser}, Parser.Str.{RawStr}, Parser.CSV.{CSV}]
    provides [main] to pf

# Until issue https://github.com/rtfeldman/roc/issues/3438 is fixed,
# use the simple 'hello world' platform for testing
# with hard-coded input.

# main = fullTest fieldContentsParser "My very cool,\"\"\r\n string"
main = partialTest fieldParser "\"An escaped field with some <- double quotes\""
# main = fullTest csvParser "10,20\n\"An escaped field!\",30\n"

partialTest = \parser, input ->
  when Parser.Str.runPartialStr parser input is
    Ok result ->
      # val = result.val |> Str.joinWith("\r\n")
      val = result.val
      leftover = result.input
      "Parse success: \(val) (leftover string: \(leftover))\n"
    Err (ParsingFailure problem) ->
      "Parse failure: \(problem)\n"

fullTest = \parser, input ->
  when Parser.Str.runStr parser input is
    Ok result ->
      # val = result |> Str.joinWith(", ")
      # val = result |> Str.joinWith("\r\n")
      val = result
      "Parse success: \(val)\n"
    Err (ParsingFailure problem) ->
      "Parse failure: \(problem)\n"
    Err (ParsingIncomplete leftover) ->
      "Parse failure: Expected to reach end of input, but the following was still left: `\(leftover)`\n"

fieldContentsParser : Parser RawStr Str
fieldContentsParser =
  Parser.CSV.escapedContents
  |> Parser.Core.map (\field -> field |> Str.fromUtf8 |> Result.withDefault "Should not happen")

fieldParser : Parser RawStr Str
fieldParser =
  Parser.CSV.escapedField
  |> Parser.Core.map (\field -> field |> Str.fromUtf8 |> Result.withDefault "Should not happen")

csvParser : Parser RawStr (List Str)
csvParser =
  Parser.CSV.file
  |> Parser.Core.map \records ->
    records
    |> List.map (\record ->
      record
      |> List.map (\field ->
        field
        |> Str.fromUtf8
        |> Result.withDefault "Unexpected problem while turning a List U8 back into a Str")
      |> Str.joinWith ", ")
