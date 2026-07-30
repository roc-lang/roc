overflow : List(Str) -> I16
overflow = |args| {
    x : I16
    x = if args.len() == 999999 { 1 } else { I16.lowest }

    I16.abs(x)
}

main! = |args| Ok(overflow(args))
