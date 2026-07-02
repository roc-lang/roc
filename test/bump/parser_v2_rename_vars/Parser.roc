## parser_v1 with all type variables alpha-renamed — a PATCH-level change.

Parser(i, v) := { run : i -> [Ok(v, i), Err(Str)] }.{

    ## Run a parser on input.
    parse : Parser(i, v), i -> [Ok(v, i), Err(Str)]
    parse = |parser, inp|
        (parser.run)(inp)

    ## Always succeed with the given value.
    succeed : v -> Parser(i, v)
    succeed = |value|
        { run: |inp| Ok(value, inp) }

    ## Always fail with the given message.
    fail : Str -> Parser(i, v)
    fail = |msg|
        { run: |_inp| Err(msg) }
}

expect Parser.parse(Parser.succeed("hi"), "hello") == Ok("hi", "hello")
expect Parser.parse(Parser.fail("oops"), "hello") == Err("oops")
