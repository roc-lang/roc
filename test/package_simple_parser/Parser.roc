## A simple parser combinator library demonstrating the record builder pattern.
##
## NOTE: This implementation is blocked by https://github.com/roc-lang/roc/issues/9129
## The type inference bug causes type variables to become rigid when captured in closures.

Parser(input, val) := { run : input -> [Ok(val, input), Err(Str)] }.{

    ## Run a parser on input.
    parse : Parser(input, val), input -> [Ok(val, input), Err(Str)]
    parse = |parser, inp|
        (parser.run)(inp)

    ## Create a parser that always succeeds with the given value.
    succeed : val -> Parser(input, val)
    succeed = |value|
        { run: |inp| Ok(value, inp) }

    ## Create a parser that always fails with the given message.
    fail : Str -> Parser(input, val)
    fail = |msg|
        { run: |_inp| Err(msg) }

    ## The key combinator for record builder pattern.
    ## Combines two parsers using a function to merge their results.
    map2 : Parser(input, a), Parser(input, b), (a, b -> c) -> Parser(input, c)
    map2 = |pa, pb, combine| {
        run: |inp|
            match parse(pa, inp) {
                Err(msg) => Err(msg)
                Ok(val_a, rest) =>
                    match parse(pb, rest) {
                        Err(msg2) => Err(msg2)
                        Ok(val_b, rest2) => Ok(combine(val_a, val_b), rest2)
                    }
            }
    }

    ## Transform the result of a parser using a function.
    map : Parser(input, a), (a -> b) -> Parser(input, b)
    map = |parser, transform| {
        run: |inp|
            match parse(parser, inp) {
                Err(msg) => Err(msg)
                Ok(val, rest) => Ok(transform(val), rest)
            }
    }

    ## Run first parser, keep its result, then run second parser and discard its result.
    ## TODO: blocked by #9129
    # skip : Parser(input, a), Parser(input, b) -> Parser(input, a)
    # skip = |first, second|
    #     map2(first, second, |a, _b| a)

    ## Run first parser, discard its result, then run second parser and keep its result.
    ## TODO: blocked by #9129
    # keep : Parser(input, a), Parser(input, b) -> Parser(input, b)
    # keep = |first, second|
    #     map2(first, second, |_a, b| b)
}

## Tests for the Parser type module

# Test basic succeed
expect Parser.parse(Parser.succeed("hi"), "hello") == Ok("hi", "hello")

# Test basic fail
expect Parser.parse(Parser.fail("oops"), "hello") == Err("oops")

# Test map2 directly - currently has TypeMismatch error, investigating
# combined = Parser.map2(Parser.succeed("a"), Parser.succeed("b"), |x, y| Str.concat(x, y))
# expect Parser.parse(combined, "input") == Ok("ab", "input")
