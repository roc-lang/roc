overflow : List(Str) -> U128
overflow = |args| {
    x : U128
    x = if args.len() == 999999 { 1 } else { U128.highest }

    U128.times(x, 2)
}

main! = |args| Ok(overflow(args))
