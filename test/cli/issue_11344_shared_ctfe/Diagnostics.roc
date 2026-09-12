Diagnostics := [].{
    observed = {
        dbg "issue11344-observed"
        1
    }

    failed = {
        expect 1 == 2
        2
    }

    crashed = crash "issue11344-crashed"
}
