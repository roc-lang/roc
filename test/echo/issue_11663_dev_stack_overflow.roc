# Regression for https://github.com/roc-lang/roc/issues/11663
M : { num : List(F64), scr : List(U8), out : List(Str), pc : U64 }

eval : M, List(U8), U64 -> M
eval = |m, b, i|
	if i >= List.len(b) {
		m
	} else {
		eval(m, b, i + 1)
	}

spin : M, List(U8), U64, U64 -> M
spin = |m, b, i, n|
	if i >= n {
		m
	} else {
		m1 = eval(m, b, 0)
		spin({ ..m1, num: List.set(m1.num, 7, 1.0) ?? crash ("oob"), pc: i }, b, i + 1, n)
	}

main! = |args| {
	n = 10000 + List.len(args)
	m0 = { num: List.repeat(0.0, 286), scr: List.repeat(32.U8, 1000), out: [], pc: 0 }
	final = spin(m0, Str.to_utf8("X=X+1 AND SOME MORE TEXT"), 0, n)
	echo!(F64.to_str(List.get(final.num, 7) ?? 0.0))
	Ok({})
}
