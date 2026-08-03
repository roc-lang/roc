overflow : List(Str) -> U8
overflow = |args| {
    x : U8
    x = if args.len() == 999999 { 1 } else { 16 }

    U8.pow(x, 2)
}

main! = |args| Ok(overflow(args))
