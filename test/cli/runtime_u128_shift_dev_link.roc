shifted : List(Str) -> U128
shifted = |args| U128.shl_wrap(1, args.len().to_u8_wrap())

main! = |args|
	if shifted(args) >= 1 {
		Ok({})
	} else {
		Err(UnexpectedZero)
	}
