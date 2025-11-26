Parser := [].{
    Result(input, a) : Try({ val : a, rem : input }, [ParsingFailure(Str)])
    Fn(input, a) : input -> Result(input, a)

    parse_partial : Fn(input, a), input -> Result(input, a)
    parse_partial = |fn, input| fn(input)
}

test_fn : Parser.Fn(List(U8), Bool)
test_fn = |bytes| if (bytes.len() > 0) Ok({ val : True, rem : [] }) else Err(ParsingFailure("no input"))

test_input : List(U8)
test_input = [1,2,3]

test_empty : List(U8)
test_empty = []

expect Parser.parse_partial(test_fn, test_empty) == Err(ParsingFailure("no input"))
expect Parser.parse_partial(test_fn, test_input) == Ok({ val : True, rem : [1] })
