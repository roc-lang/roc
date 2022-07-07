app "main"
    packages { pf: "platform/main.roc" }
    imports [Parser.Core.{Parser}]
    provides [main] to pf

# Until issue https://github.com/rtfeldman/roc/issues/3438 is fixed,
# use the simple 'hello world' platform for testing
# with hard-coded input.

main : Str
main = fullTest myparser "[aaaaaa,aaa,a,aaa]"

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
      "Parse failure: Expected to reach end of input, but the following was still left: \(leftover)\n"

myparser : Parser (List Str)
myparser =
  (Parser.Core.string "a")
  |> Parser.Core.oneOrMore
  |> Parser.Core.map (\vals -> Str.joinWith vals "")
  |> Parser.Core.sepBy (Parser.Core.scalar ',')
  |> Parser.Core.betweenBraces

  # "a"
  # |> Parser.Core.string
  # |> Parser.Core.many

  #Parser.Core.oneOrMore (Parser.Core.string "h")
  # string "h"

  # NOTE: using oneOf currently causes a StackOverflow in the compiler
  # Parser.Core.oneOf [
  #   Parser.Core.string "hello",
  #   Parser.Core.string "george",
  #   Parser.Core.string "richard",
  # ]

  # alt (string "hello") (alt (string "george") (string "richard"))

