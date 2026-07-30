## parser_v1 with `fail` removed and `map` added, alongside a new Extras
## module — a mixed diff whose rendered output the CLI tests assert in full.

Parser(input, val) := { run : input -> [Ok(val, input), Err(Str)] }.{

    ## Run a parser on input.
    parse : Parser(input, val), input -> [Ok(val, input), Err(Str)]
    parse = |parser, inp|
        (parser.run)(inp)

    ## Always succeed with the given value.
    succeed : val -> Parser(input, val)
    succeed = |value|
        { run: |inp| Ok(value, inp) }

    ## Transform the result of a parser using a function.
    map : Parser(input, a), (a -> b) -> Parser(input, b)
    map = |parser, transform| {
        run: |inp|
            match parse(parser, inp) {
                Err(msg) => Err(msg)
                Ok(val, rest) => Ok(transform(val), rest)
            }
    }
}

expect Parser.parse(Parser.succeed("hi"), "hello") == Ok("hi", "hello")
