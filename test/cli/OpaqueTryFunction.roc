OpaqueTryFunction(a) :: { run : {} -> Try(a, [Bad]) }.{
    make : ({} -> Try(a, [Bad])) -> OpaqueTryFunction(a)
    make = |thunk| {
        { run: thunk }
    }

    run : OpaqueTryFunction(a) -> Try(a, [Bad])
    run = |{ run: thunk }| thunk({})
}

expect OpaqueTryFunction.run(OpaqueTryFunction.make(|_| Ok("hi"))) == Ok("hi")
