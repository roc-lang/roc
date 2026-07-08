zero : List(Str) -> I64
zero = |args| {
    divisor : I64
    divisor = if args.len() == 999999 { 1 } else { 0 }

    I64.mod_by(10, divisor)
}

main! = |args| Ok(zero(args))
