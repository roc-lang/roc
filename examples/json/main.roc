app "main"
    packages { pf: "platform/main.roc" }
    imports [ParserCore.{Parser}, ParserStr.{RawStr}, ParserJson.{JsonValue}]
    provides [main] to pf

# Until issue https://github.com/rtfeldman/roc/issues/3438 is fixed,
# use the simple 'hello world' platform for testing
# with hard-coded input.

main : Str
main = fullTest myparser "[10,20,30,40,50,60,1234,1337,101010101]"

partialTest = \parser, input ->
  when ParserStr.runPartialStr parser input is
    Ok result ->
      val = result.val |> Str.joinWith("  --  ")
      # val = result.val
      leftover = result.input
      "Parse success: \(val) (leftover string: \(leftover))\n"
    Err (ParsingFailure problem) ->
      "Parse failure: \(problem)\n"

fullTest = \parser, input ->
  when ParserStr.runStr parser input is
    Ok result ->
      # val = result |> Str.joinWith(", ")
      val = result |> Str.joinWith("  --  ")
      # val = result
      "Parse success: \(val)\n"
    Err (ParsingFailure problem) ->
      "Parse failure: \(problem)\n"
    Err (ParsingIncomplete leftover) ->
      "Parse failure: Expected to reach end of input, but the following was still left: `\(leftover)`\n"

myparser : Parser RawStr (List Str)
myparser =
  # ParserCore.oneOf [ParserStr.string "a", ParserStr.string "b"]
  ParserJson.jsonNumArray
  # |> ParserCore.oneOrMore
  # |> ParserCore.map (\vals -> Str.joinWith vals "")
  |> ParserCore.map ParserJson.jsonValueToStrDebug # (Num.toStr)
  |> ParserCore.sepBy1 (ParserStr.scalar ',')
  |> ParserCore.between (ParserStr.scalar '[') (ParserStr.scalar ']')
