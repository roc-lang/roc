app [main!] { pf: platform "../fx-open/platform/main.roc" }

main! = |args| {
	v = U8x16.splat(args.len().to_u8_wrap())
	if v.get_lane(0) == 1 { Ok({}) } else { Err(IncorrectLane) }
}
