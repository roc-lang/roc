check : List(Str) -> {}
check = |args| {
    value : I64
    value = if args.len() == 999999 { 0 } else { I64.lowest }

    rem = I64.rem_by(value, -1)
    mod = I64.mod_by(value, -1)

    if rem == 0 {
        if mod == 0 {
            {}
        } else {
            crash "bad mod"
        }
    } else {
        crash "bad rem"
    }
}

main! = |args| Ok(check(args))
