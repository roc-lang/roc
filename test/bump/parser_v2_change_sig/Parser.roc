## parser_v1 with `fail` taking a second argument — a MAJOR change.

Parser(input, val) := { run : input -> [Ok(val, input), Err(Str)] }.{

    ## Run a parser on input.
    parse : Parser(input, val), input -> [Ok(val, input), Err(Str)]
    parse = |parser, inp|
        (parser.run)(inp)

    ## Always succeed with the given value.
    succeed : val -> Parser(input, val)
    succeed = |value|
        { run: |inp| Ok(value, inp) }

    ## Always fail with the given message and extra context.
    fail : Str, Str -> Parser(input, val)
    fail = |msg, extra|
        { run: |_inp| Err(Str.concat(msg, extra)) }
}

expect Parser.parse(Parser.succeed("hi"), "hello") == Ok("hi", "hello")
expect Parser.parse(Parser.fail("oops", "!"), "hello") == Err("oops!")
