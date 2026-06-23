underflow : List(Str) -> I128
underflow = |args| {
    x : I128
    x = if args.len() == 999999 { 0 } else { I128.lowest }

    x - 1
}

main! = |args| Ok(underflow(args))
