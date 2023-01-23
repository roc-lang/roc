interface ParserCore
    exposes [
        Parser,
        ParseResult,
        parse,
        parsePartial,
        fail,
        const,
        alt,
        keep,
        skip,
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
        ignore,
        buildPrimitiveParser,
        flatten,
    ]
    imports []

## Opaque type for a parser that will try to parse an `a` from an `input`.
##
## As a simple example, you might consider a parser that tries to parse a `U32` from a `Str`.
## Such a process might succeed or fail, depending on the current value of `input`.
##
## As such, a parser can be considered a recipe
## for a function of the type `input -> Result {val: a, input: input} [ParsingFailure Str]`.
##
## How a parser is _actually_ implemented internally is not important
## and this might change between versions;
## for instance to improve efficiency or error messages on parsing failures.
Parser input a := input -> ParseResult input a

ParseResult input a : Result { val : a, input : input } [ParsingFailure Str]

buildPrimitiveParser : (input -> ParseResult input a) -> Parser input a
buildPrimitiveParser = \fun ->
    @Parser fun

# -- Generic parsers:
## Most general way of running a parser.
##
## Can be tought of turning the recipe of a parser into its actual parsing function
## and running this function on the given input.
##
## Many (but not all!) parsers consume part of `input` when they succeed.
## This allows you to string parsers together that run one after the other:
## The part of the input that the first parser did not consume, is used by the next parser.
## This is why a parser returns on success both the resulting value and the leftover part of the input.
##
## Of course, this is mostly useful when creating your own internal parsing building blocks.
## `run` or `Parser.Str.runStr` etc. are more useful in daily usage.
parsePartial : Parser input a, input -> ParseResult input a
parsePartial = \@Parser parser, input ->
    parser input

## Runs a parser on the given input, expecting it to fully consume the input
##
## The `input -> Bool` parameter is used to check whether parsing has 'completed',
## (in other words: Whether all of the input has been consumed.)
##
## For most (but not all!) input types, a parsing run that leaves some unparsed input behind
## should be considered an error.
parse : Parser input a, input, (input -> Bool) -> Result a [ParsingFailure Str, ParsingIncomplete input]
parse = \parser, input, isParsingCompleted ->
    when parsePartial parser input is
        Ok { val: val, input: leftover } ->
            if isParsingCompleted leftover then
                Ok val
            else
                Err (ParsingIncomplete leftover)

        Err (ParsingFailure msg) ->
            Err (ParsingFailure msg)

## Parser that can never succeed, regardless of the given input.
## It will always fail with the given error message.
##
## This is mostly useful as 'base case' if all other parsers
## in a `oneOf` or `alt` have failed, to provide some more descriptive error message.
fail : Str -> Parser * *
fail = \msg ->
    @Parser \_input -> Err (ParsingFailure msg)

## Parser that will always produce the given `val`, without looking at the actual input.
## This is useful as basic building block, especially in combination with
## `map` and `keep`.
const : a -> Parser * a
const = \val ->
    @Parser \input ->
        Ok { val: val, input: input }

## Try the `first` parser and (only) if it fails, try the `second` parser as fallback.
alt : Parser input a, Parser input a -> Parser input a
alt = \first, second ->
    buildPrimitiveParser \input ->
        when parsePartial first input is
            Ok { val: val, input: rest } -> Ok { val: val, input: rest }
            Err (ParsingFailure firstErr) ->
                when parsePartial second input is
                    Ok { val: val, input: rest } -> Ok { val: val, input: rest }
                    Err (ParsingFailure secondErr) ->
                        Err (ParsingFailure ("\(firstErr) or \(secondErr)"))

## Runs a parser building a function, then a parser building a value,
## and finally returns the result of calling the function with the value.
##
## This is useful if you are building up a structure that requires more parameters
## than there are variants of `map`, `map2`, `map3` etc. for.
##
## For instance, the following two are the same:
##
## >>> const (\x, y, z -> Triple x y z)
## >>> |> map3 Parser.Str.nat Parser.Str.nat Parser.Str.nat
##
## >>> const (\x -> \y -> \z -> Triple x y z)
## >>> |> keep Parser.Str.nat
## >>> |> keep Parser.Str.nat
## >>> |> keep Parser.Str.nat
##
## (And indeed, this is how `map`, `map2`, `map3` etc. are implemented under the hood.)
##
## # Currying
## Be aware that when using `keep`, you need to explicitly 'curry' the parameters to the construction function.
## This means that instead of writing `\x, y, z -> ...`
## you'll need to write `\x -> \y -> \z -> ...`.
## This is because the parameters to the function will be applied one-by-one as parsing continues.
keep : Parser input (a -> b), Parser input a -> Parser input b
keep = \funParser, valParser ->
    @Parser \input ->
        when parsePartial funParser input is
            Ok { val: funVal, input: rest } ->
                when parsePartial valParser rest is
                    Ok { val: val, input: rest2 } ->
                        Ok { val: funVal val, input: rest2 }

                    Err e ->
                        Err e

            Err e ->
                Err e

## Skip over a parsed item as part of a pipeline
##
## This is useful if you are using a pipeline of parsers with `keep` but
## some parsed items are not part of the final result
##
## >>> const (\x -> \y -> \z -> Triple x y z)
## >>> |> keep Parser.Str.nat
## >>> |> skip (codeunit ',')
## >>> |> keep Parser.Str.nat
## >>> |> skip (codeunit ',')
## >>> |> keep Parser.Str.nat
##
skip : Parser input kept, Parser input skipped -> Parser input kept
skip = \kept, skipped ->
    @Parser \input ->
        when parsePartial kept input is
            Ok step1 ->
                when parsePartial skipped step1.input is
                    Ok step2 ->
                        Ok { val: step1.val, input: step2.input }

                    Err e ->
                        Err e

            Err e ->
                Err e

# Internal utility function. Not exposed to users, since usage is discouraged!
#
# Runs `firstParser` and (only) if it succeeds,
# runs the function `buildNextParser` on its result value.
# This function returns a new parser, which is finally run.
#
# `andThen` is usually more flexible than necessary, and less efficient
# than using `const` with `map` and/or `keep`.
# Consider using those functions first.
andThen : Parser input a, (a -> Parser input b) -> Parser input b
andThen = \@Parser firstParser, buildNextParser ->
    @Parser \input ->
        when firstParser input is
            Ok step ->
                (@Parser nextParser) = buildNextParser step.val
                nextParser step.input

            Err e ->
                Err e

## Try a list of parsers in turn, until one of them succeeds
oneOf : List (Parser input a) -> Parser input a
oneOf = \parsers ->
    List.walkBackwards parsers (fail "oneOf: The list of parsers was empty") (\laterParser, earlierParser -> alt earlierParser laterParser)

## Transforms the result of parsing into something else,
## using the given transformation function.
map : Parser input a, (a -> b) -> Parser input b
map = \@Parser simpleParser, transform ->
    @Parser \input ->
        when simpleParser input is
            Ok step ->
                Ok { val: transform step.val, input: step.input }

            Err e ->
                Err e

## Transforms the result of parsing into something else,
## using the given two-parameter transformation function.
map2 : Parser input a, Parser input b, (a, b -> c) -> Parser input c
map2 = \@Parser parserA, @Parser parserB, transform ->
    @Parser \input ->
        when parserA input is
            Ok step1 ->
                when parserB step1.input is
                    Ok step2 ->
                        Ok { val: transform step1.val step2.val, input: step2.input }

                    Err e ->
                        Err e

            Err e ->
                Err e

## Transforms the result of parsing into something else,
## using the given three-parameter transformation function.
##
## If you need transformations with more inputs,
## take a look at `keep`.
map3 : Parser input a, Parser input b, Parser input c, (a, b, c -> d) -> Parser input d
map3 = \@Parser parserA, @Parser parserB, @Parser parserC, transform ->
    @Parser \input ->
        when parserA input is
            Ok step1 ->
                when parserB step1.input is
                    Ok step2 ->
                        when parserC step2.input is
                            Ok step3 ->
                                Ok { val: transform step1.val step2.val step3.val, input: step3.input }

                            Err e ->
                                Err e

                    Err e ->
                        Err e

            Err e ->
                Err e

# ^ And this could be repeated for as high as we want, of course.
# Removes a layer of 'result' from running the parser.
#
# This allows for instance to map functions that return a result over the parser,
# where errors are turned into `ParsingFailure` s.
flatten : Parser input (Result a Str) -> Parser input a
flatten = \parser ->
    buildPrimitiveParser \input ->
        result = parsePartial parser input

        when result is
            Err problem ->
                Err problem

            Ok { val: Ok val, input: inputRest } ->
                Ok { val: val, input: inputRest }

            Ok { val: Err problem, input: _inputRest } ->
                Err (ParsingFailure problem)

## Runs a parser lazily
##
## This is (only) useful when dealing with a recursive structure.
## For instance, consider a type `Comment : { message: String, responses: List Comment }`.
## Without `lazy`, you would ask the compiler to build an infinitely deep parser.
## (Resulting in a compiler error.)
##
lazy : ({} -> Parser input a) -> Parser input a
lazy = \thunk ->
    const {}
    |> andThen thunk

maybe : Parser input a -> Parser input (Result a [Nothing])
maybe = \parser ->
    alt (parser |> map (\val -> Ok val)) (const (Err Nothing))

manyImpl : Parser input a, List a, input -> ParseResult input (List a)
manyImpl = \@Parser parser, vals, input ->
    result = parser input

    when result is
        Err _ ->
            Ok { val: vals, input: input }

        Ok { val: val, input: inputRest } ->
            manyImpl (@Parser parser) (List.append vals val) inputRest

## A parser which runs the element parser *zero* or more times on the input,
## returning a list containing all the parsed elements.
##
## Also see `oneOrMore`.
many : Parser input a -> Parser input (List a)
many = \parser ->
    @Parser \input ->
        manyImpl parser [] input

## A parser which runs the element parser *one* or more times on the input,
## returning a list containing all the parsed elements.
##
## Also see `many`.
oneOrMore : Parser input a -> Parser input (List a)
oneOrMore = \@Parser parser ->
    @Parser \input ->
        when parser input is
            Ok step ->
                manyImpl (@Parser parser) [step.val] step.input

            Err e ->
                Err e

## Runs a parser for an 'opening' delimiter, then your main parser, then the 'closing' delimiter,
## and only returns the result of your main parser.
##
## Useful to recognize structures surrounded by delimiters (like braces, parentheses, quotes, etc.)
##
## >>> betweenBraces  = \parser -> parser |> between (scalar '[') (scalar ']')
between : Parser input a, Parser input open, Parser input close -> Parser input a
between = \parser, open, close ->
    map3 open parser close (\_, val, _ -> val)

sepBy1 : Parser input a, Parser input sep -> Parser input (List a)
sepBy1 = \parser, separator ->
    parserFollowedBySep =
        const (\_ -> \val -> val)
        |> keep separator
        |> keep parser

    const (\val -> \vals -> List.prepend vals val)
    |> keep parser
    |> keep (many parserFollowedBySep)

sepBy : Parser input a, Parser input sep -> Parser input (List a)
sepBy = \parser, separator ->
    alt (sepBy1 parser separator) (const [])

ignore : Parser input a -> Parser input {}
ignore = \parser ->
    map parser (\_ -> {})
