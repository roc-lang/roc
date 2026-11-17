app [] {}

# https://github.com/roc-lang/roc/issues/13035
# The benchmark runner changes only [E] to [] for the control.
step : List(U8), U64 -> Try(List(U8), [E])
step = |out, x| Ok(out.append(x.to_u8_wrap()))

wrap : List(U8), U64 -> Try(List(U8), [E])
wrap = |out, x| {
	next = step(out, x)?
	Ok(next)
}

run : U64 -> List(U8)
run = |n| {
	var $out = List.with_capacity(1024)
	for k in (0.U64).until(n) {
		$out = match wrap($out, k) {
			Ok(v) => v
			Err(_) => []
		}
	}
	$out
}

main! = |args| {
	loops = U64.from_str(args.get(0) ?? "0") ?? 0
	var $sum = 0.U64
	for j in (0.U64).until(loops) {
		$sum = $sum.plus_wrap(run(1000 + (j % 2)).len())
	}
	echo!("${$sum.to_str()}\n")
	Ok({})
}
