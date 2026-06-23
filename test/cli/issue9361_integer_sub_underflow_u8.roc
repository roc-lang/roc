underflow : List(Str) -> U8
underflow = |args| {
    x : U8
    x = if args.len() == 999999 { 255 } else { 0 }

    x - 255
}

main! = |args| Ok(underflow(args))
