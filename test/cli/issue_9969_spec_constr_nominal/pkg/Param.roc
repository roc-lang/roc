import Base exposing [
	ParsedArg,
]
import Builder exposing [CliBuilder]

Param := [].{
	# Issue 9969: the `Err`/`Ok` constructors below are written unqualified,
	# so unification promotes them to the nominal `Try` type. Their IR
	# representation must match the nominal-wrapped `Try` patterns that the
	# `?` desugaring produces, or case-of-case rewriting in call-pattern
	# specialization concludes no branch can match.
	str_list : {} -> CliBuilder(List(Str))
	str_list = |_| {
		arg_parser = |args| {
			{ values } = extract_param_values(args)?

			Ok({ data: values, remaining_args: [] })
		}

		Builder.from_arg_parser(arg_parser)
	}

	# Control: the same parser with qualified `Try.Err`/`Try.Ok`
	# constructors, which canonicalize to explicit nominal construction.
	# Both spellings must behave identically.
	str_list_qualified : {} -> CliBuilder(List(Str))
	str_list_qualified = |_| {
		arg_parser = |args| {
			{ values } = extract_param_values_qualified(args)?

			Ok({ data: values, remaining_args: [] })
		}

		Builder.from_arg_parser(arg_parser)
	}
}

ExtractParamValuesState : {
	values : List(Str),
}

extract_param_values : List(ParsedArg) -> Try(ExtractParamValuesState, [ArgErr])
extract_param_values = |args| {
	extract_param_loop(args, { values: [] })
}

extract_param_loop : List(ParsedArg), ExtractParamValuesState -> Try(ExtractParamValuesState, [ArgErr])
extract_param_loop = |args, state|
	match args {
		[] => Ok(state)
		[arg, .. as rest] => {
			next_state =
				match arg {
					Short => Err(ArgErr)
					ShortGroup => Err(ArgErr)
					Long => Err(ArgErr)
					Parameter(p) => Ok({ ..state, values: state.values.append(p) })
				}?

			extract_param_loop(rest, next_state)
		}
	}

extract_param_values_qualified : List(ParsedArg) -> Try(ExtractParamValuesState, [ArgErr])
extract_param_values_qualified = |args| {
	extract_param_loop_qualified(args, { values: [] })
}

extract_param_loop_qualified : List(ParsedArg), ExtractParamValuesState -> Try(ExtractParamValuesState, [ArgErr])
extract_param_loop_qualified = |args, state|
	match args {
		[] => Try.Ok(state)
		[arg, .. as rest] => {
			next_state =
				match arg {
					Short => Try.Err(ArgErr)
					ShortGroup => Try.Err(ArgErr)
					Long => Try.Err(ArgErr)
					Parameter(p) => Try.Ok({ ..state, values: state.values.append(p) })
				}?

			extract_param_loop_qualified(rest, next_state)
		}
	}
