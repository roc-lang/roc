app "main"
    packages { pf: "platform/main.roc" }
    imports [Parser.Core.{Parser}]
    provides [main] to pf

# Until issue https://github.com/rtfeldman/roc/issues/3438 is fixed,
# use the simple 'hello world' platform for testing
# with hard-coded input.

main : Str
main = fullTest myparser "[aaaaaa,baabab,a,aaa]"

partialTest = \parser, input ->
  when Parser.Core.runPartialStr parser input is
    Ok result ->
      val = result.val |> Str.joinWith("  --  ")
      # val = result.val
      leftover = result.input
      "Parse success: \(val) (leftover string: \(leftover))\n"
    Err (ParsingFailure problem) ->
      "Parse failure: \(problem)\n"

fullTest = \parser, input ->
  when Parser.Core.runStr parser input is
    Ok result ->
      # val = result |> Str.joinWith(", ")
      val = result |> Str.joinWith("  --  ")
      # val = result
      "Parse success: \(val)\n"
    Err (ParsingFailure problem) ->
      "Parse failure: \(problem)\n"
    Err (ParsingIncomplete leftover) ->
      "Parse failure: Expected to reach end of input, but the following was still left: `\(leftover)`\n"

myparser : Parser Parser.Core.RawStr (List Str)
myparser =
  Parser.Core.oneOf [Parser.Core.string "a", Parser.Core.string "b"]
  |> Parser.Core.oneOrMore
  |> Parser.Core.map (\vals -> Str.joinWith vals "")
  |> Parser.Core.sepBy (Parser.Core.scalar ',')
  |> Parser.Core.between (Parser.Core.scalar '[') (Parser.Core.scalar ']')
