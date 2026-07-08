# repro for https://github.com/roc-lang/roc/issues/9973
# Two different static-dispatch methods called with `?`, next to a call to a
# helper defined lower down in the file, all inside a closure nested in another
# function. On the source-order re-entrant checker this panicked with
# "trying to add var at rank 3, but current rank is 2" (generalize.zig
# addVarToRank): the forward reference to `helper` re-entrantly checked it
# mid-body, and `helper`'s lambda-end drain of the global return-constraint
# list unified `run`'s try_operator constraints inside the sub-env.
Cmd := { name : Str }.{
    new : Str -> Cmd
    new = |n| { name: n }

    spawn : Cmd -> Try({}, [SpawnFailed, ..])
    spawn = |_c| Ok({})

    wait : Cmd -> Try({}, [WaitFailed, ..])
    wait = |_c| Ok({})
}

main! = |_| {
    run = || {
        c = Cmd.new("x")
        _ = c.spawn()?
        _ = c.wait()?
        _ = helper({})
        Ok({})
    }
    run()
}

helper = |x| Ok(x)
