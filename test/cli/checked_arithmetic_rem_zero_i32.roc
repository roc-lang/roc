zero : List(Str) -> I32
zero = |args| {
    divisor : I32
    divisor = if args.len() == 999999 { 1 } else { 0 }

    I32.rem_by(10, divisor)
}

main! = |args| Ok(zero(args))
