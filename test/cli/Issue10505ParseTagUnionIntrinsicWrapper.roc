# Repro for https://github.com/roc-lang/roc/issues/10505: a parser that calls
# ParseTagUnionSpec.parse through method syntax should lower and run.
Issue10505ParseTagUnionIntrinsicWrapper :: {}.{
	parse : Str -> Try(a, [Oops])
		where [
			a.parser_for : Parser -> (Str -> Try({ value : a, rest : Str }, [Oops])),
		]
	parse = |str| {
		T : a
		parse_tag = T.parser_for({})
		{ value, .. } = parse_tag(str)?
		Ok(value)
	}
}

Parser := {}.{
	parse_tag_union : Parser, Encoding.ParseTagUnionSpec(a), Str -> Try({ value : a, rest : Str }, [Oops])
	parse_tag_union = |parser, spec, state| {
		spec.parse({
			tag: "Friendly",
			encoding: parser,
			state,
			start_payloads: |payload_state, _count| Ok(payload_state),
			next_payload: |payload_state, _index, _count| Ok(payload_state),
			finish_payloads: |payload_state, _count| Ok(payload_state),
			missing: Oops,
		})
	}
}

expect Friendly == Issue10505ParseTagUnionIntrinsicWrapper.parse("Friendly")?
