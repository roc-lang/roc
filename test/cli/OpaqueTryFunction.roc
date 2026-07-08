OpaqueTryFunction(a) :: { run : {} -> Try(a, [Bad]) }.{
    make : ({} -> Try(a, [Bad])) -> OpaqueTryFunction(a)
    make = |run| {
        { run }
    }

    run : OpaqueTryFunction(a) -> Try(a, [Bad])
    run = |{ run }| run({})
}

expect OpaqueTryFunction.run(OpaqueTryFunction.make(|_| Ok("hi"))) == Ok("hi")
