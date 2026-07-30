zero : List(Str) -> U16
zero = |args| {
    divisor : U16
    divisor = if args.len() == 999999 { 1 } else { 0 }

    U16.div_by(10, divisor)
}

main! = |args| Ok(zero(args))
