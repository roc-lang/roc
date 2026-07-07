overflow : List(Str) -> I64
overflow = |args| {
    x : I64
    x = if args.len() == 999999 { 1 } else { 3037000500 }

    I64.times(x, x)
}

main! = |args| Ok(overflow(args))
