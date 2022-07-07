interface Json.Decoder
  exposes [
    runPartialRaw,
    runPartialStr,
    runRaw,
    runStr,
    fail,
    const,
    alt,
    andThen,
    oneOf,
    map,
    lazy
  ]
  imports []

Input : List U8
Parser a := (Input -> Result {val: a, input: Input} [ParsingFailure Str])

# -- Generic parsers:

## Runs a parser against the start of a list of scalars, allowing the parser to consume it only partially.
runPartialRaw : Parser a, Input -> Result {val: a, input: Input} [ParsingFailure Str]
runPartialRaw = \@Parser parser, input ->
  (parser input)

## Runs a parser against the start of a string, allowing the parser to consume it only partially.
##
## - If the parser succeeds, returns the resulting value as well as the leftover input.
## - If the parser fails, returns `Err (ParsingFailure msg)`
runPartialStr : Parser a, Str -> Result {val: a, input: Str} [ParsingFailure Str]
runPartialStr = \parser, input ->
  inputRaw = Str.toUtf8 input
  {val: firstVal, input: restRaw} <- Result.after (runPartialRaw parser inputRaw)
  # TODO: We're pretty certain that this is valid UTF8 at this point. Is there a better way than to resort to Result.withDefault?
  rest = Result.withDefault (Str.fromUtf8 restRaw) ""
  Ok {val: firstVal, input: rest}



## Runs a parser against a string, requiring the parser to consume it fully.
##
## - If the parser succeeds, returns `Ok val`
## - If the parser fails, returns `Err (ParsingFailure msg)`
## - If the parser succeeds but does not consume the full string, returns `Err (ParsingIncomplete leftover)`
runRaw : Parser a, Input -> Result a [ParsingFailure Str, ParsingIncomplete Input]
runRaw = \parser, input ->
  when (runPartialRaw parser input) is
    Ok {val: val, input: leftover} ->
      if List.len leftover == 0 then
        Ok val
      else
        Err (ParsingIncomplete leftover)
    Err (ParsingFailure msg) ->
      Err (ParsingFailure msg)

runStr : Parser a, Str -> Result a [ParsingFailure Str, ParsingIncomplete Str]
runStr = \parser, input ->
  inputRaw = Str.toUtf8 input
  when (runRaw parser inputRaw) is
    Ok val ->
      Ok val
    Err (ParsingFailure msg) ->
      Err (ParsingFailure msg)
    Err (ParsingIncomplete leftoverRaw) ->
      leftover = Result.withDefault (Str.fromUtf8 leftoverRaw) ""
      Err (ParsingIncomplete leftover)

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
    when (runPartialRaw left input) is
      Ok {val: val, input: rest} -> Ok {val: val, input: rest}
      Err (ParsingFailure leftErr) ->
        when (runPartialRaw right input) is
        Ok {val: val, input: rest} -> Ok {val: val, input: rest}
        Err (ParsingFailure rightErr) ->
          Err (ParsingFailure ("\(leftErr) or \(rightErr)"))
  @Parser fun

andThen : Parser a, (a -> Parser b) -> Parser b
andThen = \firstParser, buildNextParser ->
  fun = \input ->
    {val: firstVal, input: rest} <- Result.after (runPartialRaw firstParser input)
    nextParser = (buildNextParser firstVal)
    runPartialRaw nextParser rest
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

# -- Specific parsers:

char : U8, Parser U8 -> Parser U8
char = \expectedCodePoint ->
  @Parser \input ->
    {before: start, others: inputRest} = List.split input 1
    if List.isEmpty start then
        errorChar = Result.withDefault (Str.appendScalar "" (Num.intCast expectedCodePoint)) "?" # TODO: Introduce a cleaner way to do this with new builtins?
        Err (ParsingFailure "expected char `\(errorChar)` but input was empty")
    else
      if start == (List.single expectedCodePoint) then
        Ok {val: expectedCodePoint, input: inputRest}
      else
        errorChar = Result.withDefault (Str.appendScalar "" (Num.intCast expectedCodePoint)) "?" # TODO: Introduce a cleaner way to do this with new builtins?
        # actualChar = Str.appendScalar "" (Num.castInt firstCodePoint) # TODO: Introduce a cleaner way to do this with new builtins?
        Err (ParsingFailure "expected char `\(errorChar)` but found something else")
