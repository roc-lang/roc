app [main!] { pf: platform "./platform/main.roc" }

Codec := [Decoded(I64)].{
	decode : I64 -> Codec
	decode = |n| Codec.Decoded(n)

	encode : Codec -> I64
	encode = |Codec.Decoded(n)| n
}

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
	transform = make_map(|Codec.Decoded(n)| Codec.Decoded(n + 1))
	_ = transform(41)
	{}
}
