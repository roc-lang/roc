app [main!] { pf: platform "./platform/main.roc" }

decode_i64 : I64 -> I64
decode_i64 = |n| n

encode_i64 : I64 -> I64
encode_i64 = |n| n

make_map :
	(a -> b) -> (I64 -> I64)
		where [
			a.decode : I64 -> a,
			b.encode : b -> I64,
		]
make_map = |f| {
	wrapped : I64 -> I64
	wrapped = |input| {
		A : a
		value : a
		value = A.decode(input)

		output : b
		output = f(value)

		output.encode()
	}

	wrapped
}

main! : () => {}
main! = || {
	transform = make_map(|n| n + 1)
	_ = transform(41)
	{}
}
