interface Json.Decoder
  exposes [
    runPartial,
    run,
    fail,
    const,
    alt,
    andThen,
    oneOf,
    map,
    lazy
  ]
  imports []

Parser a := (Str -> Result {val: a, input: Str} [ParsingFailure Str])


## Runs a parser against the start of a string, allowing the parser to consume it only partially.
##
## - If the parser succeeds, returns the resulting value as well as the leftover input.
## - If the parser fails, returns `Err (ParsingFailure msg)`
runPartial : Parser a, Str -> Result {val: a, input: Str} [ParsingFailure Str]
runPartial = \@Parser parser, input ->
  (parser input)

## Runs a parser against a string, requiring the parser to consume it fully.
##
## - If the parser succeeds, returns `Ok val`
## - If the parser fails, returns `Err (ParsingFailure msg)`
## - If the parser succeeds but does not consume the full string, returns `Err (ParsingIncomplete leftover)`
run : Str, Parser a -> Result a [ParsingFailure Str, ParsingIncomplete Str]
run = \input, parser ->
  when (runPartial parser input) is
    Ok {val: val, input: ""} ->
      Ok val
    Ok {val: _val, input: leftover} ->
      Err (ParsingIncomplete leftover)
    Err (ParsingFailure msg) ->
      Err (ParsingFailure msg)

fail : Str -> Parser *
fail = \msg ->
  @Parser \_input -> Err (ParsingFailure msg)

const : a -> Parser a
const = \val ->
  @Parser \input ->
    Ok { val: val, input: input }

alt : Parser a, Parser a -> Parser a
alt = \left, right ->
  fun = \input ->
    when (runPartial left input) is
      Ok {val: val, input: rest} -> Ok {val: val, input: rest}
      Err (ParsingFailure leftErr) ->
        when (runPartial right input) is
        Ok {val: val, input: rest} -> Ok {val: val, input: rest}
        Err (ParsingFailure rightErr) ->
          Err (ParsingFailure ("\(leftErr) or \(rightErr)"))
  @Parser fun

andThen : Parser a, (a -> Parser b) -> Parser b
andThen = \firstParser, buildNextParser ->
  fun = \input ->
    {val: firstVal, input: rest} <- Result.after (runPartial firstParser input)
    nextParser = (buildNextParser firstVal)
    runPartial nextParser rest
  @Parser fun

oneOf : List (Parser a) -> Parser a
oneOf = \parsers ->
  List.walk parsers (fail "Always fail") alt

map : (a -> b), Parser a -> Parser b
map = \transform, simpleParser ->
  andThen simpleParser \result ->
    const (transform result)

lazy : ({} -> Parser a) -> Parser a
lazy = \thunk ->
  andThen (const {}) thunk
