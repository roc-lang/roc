overflow : List(Str) -> I64
overflow = |args| {
    x : I64
    x = if args.len() == 999999 { 1 } else { I64.lowest }

    I64.div_by(x, -1)
}

main! = |args| Ok(overflow(args))
