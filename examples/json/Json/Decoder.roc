interface Json.Decoder
  exposes [
    fail,
    const,
    alt,
    andThen,
    runPartial,
    run
  ]
  imports []

Parser a := (Str -> Result {val: a, input: Str} [ParsingFailure Str])

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


runPartial : Parser a, Str -> Result {val: a, input: Str} [ParsingFailure Str]
runPartial = \@Parser parser, input ->
  (parser input)

run : Str, Parser a -> Result a [ParsingFailure Str, ParsingIncomplete Str]
run = \input, parser ->
  when (runPartial parser input) is
    Ok {val: val, input: ""} ->
      Ok val
    Ok {val: _val, input: leftover} ->
      Err (ParsingIncomplete leftover)
    Err (ParsingFailure msg) ->
      Err (ParsingFailure msg)
