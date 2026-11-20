# repro for https://github.com/roc-lang/roc/issues/13245
D : { unscaled : I128, scale : I8 }

sum : D, D -> I128
sum = |a, b| {
	(low, high) = if (a.scale < b.scale) (a, b) else (b, a)
	if low.unscaled < 1000 {
		return low.unscaled * 10 + high.unscaled
	}
	0
}

main! = |args| {
	# Keep the fields dynamic so the early return runs in the generated code.
	n = args.len().to_i128()
	a = { unscaled: 125 + n, scale: 2 }
	b = { unscaled: 35 + n, scale: 1 }
	echo!("${sum(a, b).to_str()}\n")
	Ok({})
}
