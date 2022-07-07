app "main"
    packages { pf: "platform/main.roc" }
    imports [Parser.{string, oneOf, alt, andThen}]
    provides [main] to pf

# Until issue https://github.com/rtfeldman/roc/issues/3438 is fixed,
# use the simple 'hello world' platform for testing
# with hard-coded input.


main =
  when Parser.runPartialStr myparser input is
    Ok result ->
      "Parse success: \(result.val)\n"
    Err (ParsingFailure problem) ->
      "Parse failure: \(problem)\n"

input = "hello world"

myparser =
  # NOTE: using oneOf currently causes a StackOverflow in the compiler
  # oneOf [
  #   string "hello",
  #   string "george",
  #   string "richard",
  # ]
  alt (string "hello") (alt (string "george") (string "richard"))
