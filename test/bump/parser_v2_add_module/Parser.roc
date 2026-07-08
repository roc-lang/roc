## Baseline fixture for roc bump tests. The v2 sibling packages differ from
## this one in exactly one way each; the CLI tests assert the reported bump.

Parser(input, val) := { run : input -> [Ok(val, input), Err(Str)] }.{

    ## Run a parser on input.
    parse : Parser(input, val), input -> [Ok(val, input), Err(Str)]
    parse = |parser, inp|
        (parser.run)(inp)

    ## Always succeed with the given value.
    succeed : val -> Parser(input, val)
    succeed = |value|
        { run: |inp| Ok(value, inp) }

    ## Always fail with the given message.
    fail : Str -> Parser(input, val)
    fail = |msg|
        { run: |_inp| Err(msg) }
}

expect Parser.parse(Parser.succeed("hi"), "hello") == Ok("hi", "hello")
expect Parser.parse(Parser.fail("oops"), "hello") == Err("oops")
