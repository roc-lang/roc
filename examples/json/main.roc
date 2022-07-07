app "main"
    packages { pf: "platform/main.roc" }
    imports [Parser.Core.{Parser}]
    provides [main] to pf

# Until issue https://github.com/rtfeldman/roc/issues/3438 is fixed,
# use the simple 'hello world' platform for testing
# with hard-coded input.

main : Str
main =
  when Parser.Core.runPartialStr myparser input is
    Ok result ->
      # resultStr = x.val |> Str.joinWith(", ")
      val = result.val
      "Parse success: \(val)\n"
    Err (ParsingFailure problem) ->
      "Parse failure: \(problem)\n"

input : Str
input = "aaaaaa"

myparser : Parser Str
myparser = Parser.Core.many "a"
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

