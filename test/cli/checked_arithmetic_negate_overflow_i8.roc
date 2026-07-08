overflow : List(Str) -> I8
overflow = |args| {
    x : I8
    x = if args.len() == 999999 { 1 } else { I8.lowest }

    I8.negate(x)
}

main! = |args| Ok(overflow(args))
