interface Parser.Str
    exposes [
        RawStr,
        parseStr,
        parseStrPartial,
        parseRawStr,
        parseRawStrPartial,
        string,
        stringRaw,
        codeunit,
        codeunitSatisfies,
        anyString,
        anyRawString,
        anyCodeunit,
        scalar,
        oneOf,
        digit,
        positiveInt,
        strFromRaw,
    ]
    imports [Parser.Core.{ Parser, ParseResult, map, oneOrMore, parse, parsePartial, buildPrimitiveParser }]

# Specific string-based parsers:
RawStr : List U8

strFromRaw : RawStr -> Str
strFromRaw = \rawStr ->
    rawStr
    |> Str.fromUtf8
    |> Result.withDefault "Unexpected problem while turning a List U8 (that was originally a Str) back into a Str. This should never happen!"

strToRaw : Str -> RawStr
strToRaw = \str ->
    str |> Str.toUtf8

strFromScalar : U32 -> Str
strFromScalar = \scalarVal ->
    Str.appendScalar "" (Num.intCast scalarVal)
    |> Result.withDefault "Unexpected problem while turning a U32 (that was probably originally a scalar constant) into a Str. This should never happen!"

strFromCodeunit : U8 -> Str
strFromCodeunit = \cu ->
    strFromRaw [cu]

## Runs a parser against the start of a list of scalars, allowing the parser to consume it only partially.
parseRawStrPartial : Parser RawStr a, RawStr -> ParseResult RawStr a
parseRawStrPartial = \parser, input ->
    parsePartial parser input

## Runs a parser against the start of a string, allowing the parser to consume it only partially.
##
## - If the parser succeeds, returns the resulting value as well as the leftover input.
## - If the parser fails, returns `Err (ParsingFailure msg)`
parseStrPartial : Parser RawStr a, Str -> ParseResult Str a
parseStrPartial = \parser, input ->
    parser
    |> parseRawStrPartial (strToRaw input)
    |> Result.map \{ val: val, input: restRaw } ->
        { val: val, input: strFromRaw restRaw }

## Runs a parser against a string, requiring the parser to consume it fully.
##
## - If the parser succeeds, returns `Ok val`
## - If the parser fails, returns `Err (ParsingFailure msg)`
## - If the parser succeeds but does not consume the full string, returns `Err (ParsingIncomplete leftover)`
parseRawStr : Parser RawStr a, RawStr -> Result a [ParsingFailure Str, ParsingIncomplete RawStr]
parseRawStr = \parser, input ->
    parse parser input (\leftover -> List.len leftover == 0)

parseStr : Parser RawStr a, Str -> Result a [ParsingFailure Str, ParsingIncomplete Str]
parseStr = \parser, input ->
    parser
    |> parseRawStr (strToRaw input)
    |> Result.mapErr \problem ->
        when problem is
            ParsingFailure msg ->
                ParsingFailure msg

            ParsingIncomplete leftoverRaw ->
                ParsingIncomplete (strFromRaw leftoverRaw)

codeunitSatisfies : (U8 -> Bool) -> Parser RawStr U8
codeunitSatisfies = \check ->
    buildPrimitiveParser \input ->
        { before: start, others: inputRest } = List.split input 1

        when List.get start 0 is
            Err OutOfBounds ->
                Err (ParsingFailure "expected a codeunit satisfying a condition, but input was empty.")

            Ok startCodeunit ->
                if check startCodeunit then
                    Ok { val: startCodeunit, input: inputRest }
                else
                    otherChar = strFromCodeunit startCodeunit
                    inputStr = strFromRaw input

                    Err (ParsingFailure "expected a codeunit satisfying a condition but found `\(otherChar)`.\n While reading: `\(inputStr)`")

# Implemented manually instead of on top of codeunitSatisfies
# because of better error messages
codeunit : U8 -> Parser RawStr U8
codeunit = \expectedCodeUnit ->
    buildPrimitiveParser \input ->
        { before: start, others: inputRest } = List.split input 1

        when List.get start 0 is
            Err OutOfBounds ->
                errorChar = strFromCodeunit expectedCodeUnit

                Err (ParsingFailure "expected char `\(errorChar)` but input was empty.")

            Ok startCodeunit ->
                if startCodeunit == expectedCodeUnit then
                    Ok { val: expectedCodeUnit, input: inputRest }
                else
                    errorChar = strFromCodeunit expectedCodeUnit
                    otherChar = strFromRaw start
                    inputStr = strFromRaw input

                    Err (ParsingFailure "expected char `\(errorChar)` but found `\(otherChar)`.\n While reading: `\(inputStr)`")

# Implemented manually instead of a sequence of codeunits
# because of efficiency and better error messages
stringRaw : List U8 -> Parser RawStr (List U8)
stringRaw = \expectedString ->
    buildPrimitiveParser \input ->
        { before: start, others: inputRest } = List.split input (List.len expectedString)

        if start == expectedString then
            Ok { val: expectedString, input: inputRest }
        else
            errorString = strFromRaw expectedString
            otherString = strFromRaw start
            inputString = strFromRaw input

            Err (ParsingFailure "expected string `\(errorString)` but found `\(otherString)`.\nWhile reading: \(inputString)")

string : Str -> Parser RawStr Str
string = \expectedString ->
    strToRaw expectedString
    |> stringRaw
    |> map \_val -> expectedString

scalar : U32 -> Parser RawStr U32
scalar = \expectedScalar ->
    expectedScalar
    |> strFromScalar
    |> string
    |> map \_ -> expectedScalar

# Matches any codeunit
anyCodeunit : Parser RawStr U8
anyCodeunit = codeunitSatisfies (\_ -> Bool.true)

# Matches any bytestring
# and consumes all of it.
# Does not fail.
anyRawString : Parser RawStr RawStr
anyRawString = buildPrimitiveParser \rawStringValue ->
    Ok { val: rawStringValue, input: [] }

# Matches any string
# as long as it is valid UTF8.
anyString : Parser RawStr Str
anyString = buildPrimitiveParser \fieldRawString ->
    when Str.fromUtf8 fieldRawString is
        Ok stringVal ->
            Ok { val: stringVal, input: [] }

        Err (BadUtf8 _ _) ->
            Err (ParsingFailure "Expected a string field, but its contents cannot be parsed as UTF8.")

# betweenBraces : Parser RawStr a -> Parser RawStr a
# betweenBraces = \parser ->
#   between parser (scalar '[') (scalar ']')
digit : Parser RawStr U8
digit =
    digitParsers =
        List.range { start: At '0', end: At '9' }
        |> List.map \digitCodeUnit ->
            digitCodeUnit
            |> codeunit
            |> map \_ -> digitCodeUnit - '0'

    oneOf digitParsers

# NOTE: Currently happily accepts leading zeroes
positiveInt : Parser RawStr (Int *)
positiveInt =
    oneOrMore digit
    |> map \digitsList ->
        digitsList
        |> List.map \char -> Num.intCast char - '0'
        |> List.walk 0 \sum, digitVal -> 10 * sum + digitVal

## Try a bunch of different parsers.
##
## The first parser which is tried is the one at the front of the list,
## and the next one is tried until one succeeds or the end of the list was reached.
##
## >>> boolParser : Parser RawStr Bool
## >>> boolParser = oneOf [string "true", string "false"] |> map (\x -> if x == "true" then True else False)
# NOTE: This implementation works, but is limited to parsing strings.
# Blocked until issue #3444 is fixed.
oneOf : List (Parser RawStr a) -> Parser RawStr a
oneOf = \parsers ->
    buildPrimitiveParser \input ->
        List.walkUntil parsers (Err (ParsingFailure "(no possibilities)")) \_, parser ->
            when parseRawStrPartial parser input is
                Ok val ->
                    Break (Ok val)

                Err problem ->
                    Continue (Err problem)
