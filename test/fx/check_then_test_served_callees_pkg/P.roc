P(a) :: { fun : List(U8) -> Try({ value : a, rest : List(U8) }, [Fail]) }.{
	const : a -> P(a)
	const = |value| { fun: |input| Ok({ value, rest: input }) }

	map : P(a), (a -> b) -> P(b)
	map = |parser, transform| {
		fun: |input| match (parser.fun)(input) {
			Ok({ value, rest }) => Ok({ value: transform(value), rest })
			Err(e) => Err(e)
		},
	}

	keep : P((a -> b)), P(a) -> P(b)
	keep = |fun_parser, val_parser| {
		fun: |input| match (fun_parser.fun)(input) {
			Err(e) => Err(e)
			Ok({ value: f, rest }) => match (val_parser.fun)(rest) {
				Err(e) => Err(e)
				Ok(next) => Ok({ value: f(next.value), rest: next.rest })
			}
		},
	}

	skip : P(a), P(_) -> P(a)
	skip = |fun_parser, skip_parser| {
		fun: |input| match (fun_parser.fun)(input) {
			Err(e) => Err(e)
			Ok({ value, rest }) => match (skip_parser.fun)(rest) {
				Err(e) => Err(e)
				Ok(next) => Ok({ value, rest: next.rest })
			}
		},
	}

	chomp_while : (U8 -> Bool) -> P(List(U8))
	chomp_while = |check| {
		fun: |input| {
			index = input.fold_until(0, |i, elem| if check(elem) Continue(i + 1) else Break(i))
			Ok({ value: input.sublist({ start: 0, len: index }), rest: input.drop_first(index) })
		},
	}

	byte : U8 -> P(U8)
	byte = |expected| {
		fun: |input| match input {
			[first, .. as rest] if first == expected => Ok({ value: first, rest })
			_ => Err(Fail)
		},
	}

	digits : P(U64)
	digits = {
		fun: |input| {
			n = input.fold_until(0, |i, d| if d >= '0' and d <= '9' Continue(i + 1) else Break(i))
			if n == 0 {
				Err(Fail)
			} else {
				value = input.sublist({ start: 0, len: n }).fold(0, |sum, d| sum * 10 + (d - '0').to_u64())
				Ok({ value, rest: input.drop_first(n) })
			}
		},
	}

	parse_str : P(a), Str -> Try(a, [Fail])
	parse_str = |parser, text| match (parser.fun)(text.to_utf8()) {
		Ok({ value, rest: [] }) => Ok(value)
		_ => Err(Fail)
	}
}
