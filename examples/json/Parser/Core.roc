interface Parser.Core
  exposes [
    Parser,
    RawStr,
    runPartialRaw,
    runPartialStr,
    runRaw,
    runStr,
    fail,
    const,
    alt,
    apply,
    andThen,
    oneOf,
    map,
    map2,
    map3,
    lazy,
    maybe,
    oneOrMore,
    many,
    between,
    sepBy,
    sepBy1,
    codepoint,
    stringRaw,
    string,
    scalar,
    betweenBraces, # An example
  ]
  imports []


RawStr : List U8
Parser input a := (input -> Result {val: a, input: input} [ParsingFailure Str])

# -- Generic parsers:

runPartial : Parser input a, input -> Result {val: a, input: input} [ParsingFailure Str]
runPartial = \@Parser parser, input ->
  (parser input)

## Runs a parser against the start of a list of scalars, allowing the parser to consume it only partially.
runPartialRaw : Parser RawStr a, RawStr -> Result {val: a, input: RawStr} [ParsingFailure Str]
runPartialRaw = \parser, input ->
  runPartial parser input


## Runs a parser against the start of a string, allowing the parser to consume it only partially.
##
## - If the parser succeeds, returns the resulting value as well as the leftover input.
## - If the parser fails, returns `Err (ParsingFailure msg)`
runPartialStr : Parser RawStr a, Str -> Result {val: a, input: Str} [ParsingFailure Str]
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
runRaw : Parser RawStr a, RawStr -> Result a [ParsingFailure Str, ParsingIncomplete RawStr]
runRaw = \parser, input ->
  when (runPartialRaw parser input) is
    Ok {val: val, input: leftover} ->
      if List.len leftover == 0 then
        Ok val
      else
        Err (ParsingIncomplete leftover)
    Err (ParsingFailure msg) ->
      Err (ParsingFailure msg)

runStr : Parser RawStr a, Str -> Result a [ParsingFailure Str, ParsingIncomplete Str]
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

fail : Str -> Parser * *
fail = \msg ->
  @Parser \_input -> Err (ParsingFailure msg)

const : a -> Parser * a
const = \val ->
  @Parser \input ->
    Ok { val: val, input: input }

alt : Parser input a, Parser input a -> Parser input a
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

andThen : Parser input a, (a -> Parser input b) -> Parser input b
andThen = \firstParser, buildNextParser ->
  fun = \input ->
    {val: firstVal, input: rest} <- Result.after (runPartial firstParser input)
    nextParser = (buildNextParser firstVal)
    runPartial nextParser rest
  @Parser fun

applyOld : Parser input a, Parser input (a -> b) -> Parser input b
applyOld = \valParser, funParser ->
  combined = \input ->
    {val: val, input: rest} <- Result.after (runPartial valParser input)
    (runPartial funParser rest)
    |> Result.map \{val: funVal, input: rest2} ->
      {val: funVal val, input: rest2}
  @Parser combined

apply : Parser input (a -> b), Parser input a -> Parser input b
apply = \funParser, valParser ->
  combined = \input ->
    {val: funVal, input: rest} <- Result.after (runPartial funParser input)
    (runPartial valParser rest)
    |> Result.map \{val: val, input: rest2} ->
      {val: funVal val, input: rest2}
  @Parser combined


# NOTE: Using this implementation in an actual program,
# currently causes a compile-time StackOverflow (c.f. https://github.com/rtfeldman/roc/issues/3444 )
oneOfBroken : List (Parser input a) -> Parser input a
oneOfBroken = \parsers ->
  List.walkBackwards parsers (fail "Always fail") (\laterParser, earlierParser -> alt earlierParser laterParser)

oneOfBroken2 : List (Parser input a) -> Parser input a
oneOfBroken2 = \parsers ->
  if List.isEmpty parsers then
    fail "(always fail)"
  else
    firstParser = List.get parsers (List.len parsers - 1) |> Result.withDefault (fail "this should never happen!!")
    alt firstParser (oneOfBroken2 (List.dropLast parsers))

# NOTE: This implementation works, but is limited to parsing strings.
# Blocked until issue #3444 is fixed.
oneOf : List (Parser RawStr a) -> Parser RawStr a
oneOf = \parsers ->
  @Parser \input ->
    List.walkUntil parsers (Err (ParsingFailure "(no possibilities)")) \_, parser ->
      when runPartialRaw parser input is
        Ok val ->
          Break (Ok val)
        Err problem ->
          Continue (Err problem)

map : Parser input a, (a -> b) -> Parser input b
map = \simpleParser, transform ->
  const transform
  |> apply simpleParser

map2 : Parser input a, Parser input b, (a, b -> c) -> Parser input c
map2 = \parserA, parserB, transform ->
  const (\a -> \b -> transform a b)
  |> apply parserA
  |> apply parserB

map3 : Parser input a, Parser input b, Parser input c, (a, b, c-> d) -> Parser input d
map3 = \parserA, parserB, parserC, transform ->
  const (\a -> \b -> \c -> transform a b c)
  |> apply parserA
  |> apply parserB
  |> apply parserC

# ^ And this could be repeated for as high as we want, of course.


lazy : ({} -> Parser input a) -> Parser input a
lazy = \thunk ->
  andThen (const {}) thunk

maybe : Parser input a -> Parser input (Result a [Nothing])
maybe = \parser ->
  alt (parser |> map (\val -> Ok val)) (const (Err Nothing))

manyImpl : Parser input a, List a, input -> Result { input : input, val : List a } [ParsingFailure Str]
manyImpl = \parser, vals, input ->
  result = runPartial parser input
  when result is
    Err _ ->
      Ok {val: vals, input: input}
    Ok {val: val, input: inputRest} ->
      manyImpl parser (List.append vals val) inputRest

many : Parser input a -> Parser input (List a)
many = \parser ->
  @Parser \input ->
    manyImpl parser [] input

oneOrMore : Parser input a -> Parser input (List a)
oneOrMore = \parser ->
  const (\val -> \vals -> List.prepend vals val)
  |> apply parser
  |> apply (many parser)
  #  moreParser : Parser (a -> (List a))
  #  moreParser =
  #      many parser
  #      |> map (\vals -> (\val -> List.prepend vals val))
  #  apply parser moreParser

  #  val <- andThen parser
  #  parser
  #  |> many
  #  |> map (\vals -> List.prepend vals val)

#  betweenBraces : Parser input a -> Parser input a
#  betweenBraces = \parser ->
#    string "["
#    |> applyOld (parser |> map (\res -> \_ -> res))
#    |> applyOld (string "]" |> map (\_ -> \res -> res))

between : Parser input open, Parser input close, Parser input a -> Parser input a
between = \open, close, parser ->
  const (\_ -> \val -> \_ -> val)
  |> apply open
  |> apply parser
  |> apply close

betweenBraces : Parser RawStr a -> Parser RawStr a
betweenBraces = \parser ->
  between (scalar '[') (scalar ']') parser

sepBy1 : Parser input a, Parser input sep -> Parser input (List a)
sepBy1 = \parser, separator ->
  parserFollowedBySep =
    const (\_ -> \val -> val)
    |> apply separator
    |> apply parser
  const (\val -> \vals -> List.prepend vals val)
  |> apply parser
  |> apply (many parserFollowedBySep)

sepBy : Parser input a, Parser input sep -> Parser input (List a)
sepBy = \parser, separator ->
  alt (sepBy1 parser separator) (const [])
# -- Specific parsers:

codepoint : U8 -> Parser RawStr U8
codepoint = \expectedCodePoint ->
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
        otherChar = Result.withDefault (Str.fromUtf8 start) "?"
        inputStr = Result.withDefault (Str.fromUtf8 input) ""
        Err (ParsingFailure "expected char `\(errorChar)` but found `\(otherChar)`.\n While reading: `\(inputStr)`")

stringRaw : List U8 -> Parser RawStr (List U8)
stringRaw = \expectedString ->
  @Parser \input ->
    {before: start, others: inputRest} = List.split input (List.len expectedString)
    if start == expectedString then
      Ok {val: expectedString, input: inputRest}
    else
      errorString = Result.withDefault (Str.fromUtf8 expectedString) ""
      otherString = Result.withDefault (Str.fromUtf8 start) "?"
      inputString = Result.withDefault (Str.fromUtf8 input) ""
      Err (ParsingFailure "expected string `\(errorString)` but found `\(otherString)`.\nWhile reading: \(inputString)")

string : Str -> Parser RawStr Str
string = \expectedString ->
  (Str.toUtf8 expectedString)
  |> stringRaw
  |> map (\_val -> expectedString)

scalar : U32 -> Parser RawStr U32
scalar = \expectedScalar ->
  ""
  |> Str.appendScalar expectedScalar
  |> Result.map (\str -> str |> string |> map (\_ -> expectedScalar))
  |> Result.mapErr \_ ->
    num = Num.toStr expectedScalar
    fail "Invalid scalar value in scalar parser construction \(num)"
  |> collapseResult

collapseResult : Result a a -> a
collapseResult = \result ->
  when result is
    Ok val ->
      val
    Err val ->
      val
