overflow : List(Str) -> U8
overflow = |args| {
    x : U8
    x = if args.len() == 999999 { 0 } else { 255 }

    x + 255
}

main! = |args| Ok(overflow(args))
