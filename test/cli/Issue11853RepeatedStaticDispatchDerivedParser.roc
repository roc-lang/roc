app [main!] {}

Problem : [Bad, MissingRequiredField(Str)]

## A row: column names and their I64 values, read left to right.
State : { names : List(Str), values : List(I64), next : U64 }

Enc :: [Default].{
	rename_field : Enc, Str -> Str
	rename_field = |_, name| name

	parse_record_start : Enc, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], Problem)
	parse_record_start = |_, state| Ok(Uncounted(state))

	parse_i64 : Enc, State -> Try({ value : I64, rest : State }, Problem)
	parse_i64 = |_, state| {
		value = state.values.get(state.next - 1) ? |_| Bad
		Ok({ value, rest: state })
	}

	invalid_value : Enc, State -> Problem
	invalid_value = |_, _| Bad

	parse_record_field : Enc,
	Encoding.FieldName.FieldNames(_shape),
	State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue(State),
			Done(State),
		],
		Problem,
	)
	parse_record_field = |_, _fields, state|
		if state.next >= state.names.len() {
			Ok(Done(state))
		} else {
			name = state.names.get(state.next) ? |_| Bad
			Ok(TryField({ name, rest: { ..state, next: state.next + 1 } }))
		}

	parse_record_after_field : Enc, State -> Try([Continue(State), Done(State)], Problem)
	parse_record_after_field = |_, state| if state.next >= state.names.len() Ok(Done(state)) else Ok(Continue(state))

	skip_record_field : Enc, State -> Try(State, Problem)
	skip_record_field = |_, state| Ok(state)
}

Db := [D].{
	## Decodes one row into whatever record type the caller asks for.
	get : Db, State -> Try(row, Problem)
		where [row.parser_for : Enc -> (State -> Try({ value : row, rest : State }, Problem))]
	get = |_db, state| {
		Row : row
		parse = Row.parser_for(Enc.Default)
		parsed = parse(state)?
		Ok(parsed.value)
	}
}

## Unannotated, so both `db.get` calls resolve through one constrained `get` evidence slot.
helper = |db| {
	a : { n : I64 }
	a = db.get({ names: ["n"], values: [1], next: 0 }) ?? { n: 0 }
	b : { c : I64 }
	b = db.get({ names: ["c"], values: [2], next: 0 }) ?? { c: 0 }
	Str.inspect((a, b))
}

main! = |_args| {
	echo!(helper(Db.D))
	Ok({})
}
