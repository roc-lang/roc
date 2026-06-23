overflow : List(Str) -> U128
overflow = |args| {
    x : U128
    x = if args.len() == 999999 { 0 } else { U128.highest }

    x + 1
}

main! = |args| Ok(overflow(args))
