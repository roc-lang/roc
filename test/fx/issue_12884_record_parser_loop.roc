app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# A record field read through a custom `parser_for` whose loop updates two
# variables and returns one of them. The record's empty field slots must stay
# recognizable as empty across that loop in optimized builds.
main! = || {
	input = Stdin.line!()
	Shape : { field : Reader }
	parse_shape = Shape.parser_for(Format.Default)
	match parse_shape(input) {
		Ok({ value: { field: Reader.(c) }, rest: _ }) => Stdout.line!(c)
		Err(_) => Stdout.line!("failed")
	}
}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| name

	missing_field : Format, Str -> [Rejected(Str)]
	missing_field = |_, name| Rejected(name)

	parse_record_start : Format, Str -> Try([Counted({ len : U64, rest : Str }), Uncounted(Str)], [Rejected(Str)])
	parse_record_start = |_, state| Ok(Counted({ len: 1, rest: state }))

	parse_record_field : Format,
	Encoding.FieldName.FieldNames(_shape),
	Str -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : Str }),
			TryField({ name : Str, rest : Str }),
			TryFieldCaseless({ name : Str, rest : Str }),
			Continue(Str),
			Done(Str),
		],
		[Rejected(Str)],
	)
	parse_record_field = |_, _, state| Ok(TryField({ name: "field", rest: state }))

	parse_record_after_field : Format, Str -> Try([Continue(Str), Done(Str)], [Rejected(Str)])
	parse_record_after_field = |_, state| Ok(Continue(state))

	skip_record_field : Format, Str -> Try(Str, [Rejected(Str)])
	skip_record_field = |_, state| Ok(state)
}

Reader := Str.{
	parser_for : Format -> (Str -> Try({ value : Reader, rest : Str }, [Rejected(Str)]))
	parser_for = |_| |state| {
		var $count = 0
		var $last = ""
		for part in [state] {
			$count = $count + 1
			$last = part
		}
		Ok({ value: Reader.($last), rest: state })
	}
}
