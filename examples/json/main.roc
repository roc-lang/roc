app "main"
    packages { pf: "platform/main.roc" }
    imports [Parser.Core]
    provides [main] to pf

# Until issue https://github.com/rtfeldman/roc/issues/3438 is fixed,
# use the simple 'hello world' platform for testing
# with hard-coded input.

main : Str
main =
  when Parser.Core.runPartialStr myparser input is
    Ok x ->
      # resultStr = result.val |> Str.joinWith(", ")
      "Parse success: \(x)\n"
    Err (ParsingFailure problem) ->
      "Parse failure: \(problem)\n"

input : Str
input = "aaaaaa"

myparser : Parser (List Str)
myparser =
  "a"
  |> Parser.Core.string
  |> Parser.Core.many

  #Parser.Core.oneOrMore (Parser.Core.string "h")
  # string "h"

  # NOTE: using oneOf currently causes a StackOverflow in the compiler
  # oneOf [
  #   string "hello",
  #   string "george",
  #   string "richard",
  # ]

  # alt (string "hello") (alt (string "george") (string "richard"))

