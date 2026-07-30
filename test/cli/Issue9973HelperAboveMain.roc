# positive control for #9973: the identical shape with the helper defined
# ABOVE main!, so the reference is backward in source order. Checked clean on
# the source-order driver and must stay clean on the graph-order driver.
Cmd := { name : Str }.{
    new : Str -> Cmd
    new = |n| { name: n }

    spawn : Cmd -> Try({}, [SpawnFailed, ..])
    spawn = |_c| Ok({})

    wait : Cmd -> Try({}, [WaitFailed, ..])
    wait = |_c| Ok({})
}

helper = |x| Ok(x)

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
