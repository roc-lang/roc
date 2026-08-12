twice_direct : U64 -> U64
twice_direct = |value| {
    local = |n| n * 2
    local(value)
}

twice_erased : U64 -> U64
twice_erased = |value| {
    local = |n| n * 2
    apply = |fn, arg| fn(arg)
    apply(local, value)
}

twice_both : U64, Dec -> { int: U64, dec: Dec }
twice_both = |int_value, dec_value| {
    local = |n| n * 2
    apply = |fn, arg| fn(arg)
    {
        int: apply(local, int_value),
        dec: apply(local, dec_value),
    }
}

main! = |args| {
    value = List.len(args)
    expected = value + value
    dec_value = U64.to_dec(value)
    both = twice_both(value, dec_value)
    if twice_direct(value) != expected or twice_erased(value) != expected or both.int != expected or both.dec != dec_value + dec_value {
        crash "literal evidence used the wrong runtime descriptor"
    }
    Ok({})
}
