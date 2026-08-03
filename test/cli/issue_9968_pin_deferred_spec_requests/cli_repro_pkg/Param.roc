import Base exposing [
	ParsedArg,
]
import Builder exposing [CliBuilder]

# Phantom type argument bound to a nested nominal type (type-level only).
PhantomParams := { get_params : {} }

Param := [].{
	builder_with_parameter_parser : [Optional, Many], (List(Str) -> Try(data, [ArgErr])) -> CliBuilder(data, from_action, to_action)
	builder_with_parameter_parser = |param, value_parser| {
		arg_parser = |args| {
			{ values, remaining_args, .. } = extract_param_values({ args, param })?
			data = value_parser(values)?

			Ok({ data, remaining_args })
		}

		Builder.from_arg_parser(arg_parser)
	}

	# The issue's optional parameter: phantom `to_action` bound to a
	# non-empty record row that exists only in this annotation.
	maybe_str : {} -> CliBuilder(Try(Str, [NoValue]), { ..action }, { get_params : {} })
	maybe_str = |_| {
		value_parser = |values|
			match values {
				[] => Ok(Err(NoValue))
				[single_value, ..] => Ok(Ok(single_value))
			}

		Param.builder_with_parameter_parser(Optional, value_parser)
	}

	# Control: same phantom record row, but the value argument is I64
	# instead of {} (guards against positional/slot-pairing regressions).
	maybe_str_i64 : I64 -> CliBuilder(Try(Str, [NoValue]), { ..action }, { get_params : {} })
	maybe_str_i64 = |_| {
		value_parser = |values|
			match values {
				[] => Ok(Err(NoValue))
				[single_value, ..] => Ok(Ok(single_value))
			}

		Param.builder_with_parameter_parser(Optional, value_parser)
	}

	# Phantom `to_action` bound to a non-empty tag row.
	maybe_str_tag : {} -> CliBuilder(Try(Str, [NoValue]), from_action, [GetParams({})])
	maybe_str_tag = |_| {
		value_parser = |values|
			match values {
				[] => Ok(Err(NoValue))
				[single_value, ..] => Ok(Ok(single_value))
			}

		Param.builder_with_parameter_parser(Optional, value_parser)
	}

	# Phantom `to_action` bound to a nested nominal type.
	maybe_str_nominal : {} -> CliBuilder(Try(Str, [NoValue]), from_action, PhantomParams)
	maybe_str_nominal = |_| {
		value_parser = |values|
			match values {
				[] => Ok(Err(NoValue))
				[single_value, ..] => Ok(Ok(single_value))
			}

		Param.builder_with_parameter_parser(Optional, value_parser)
	}

	# The issue's rest parameter: open `from_action` record row shares a
	# type variable with the preceding builder's phantom `to_action`.
	str_list : {} -> CliBuilder(List(Str), { ..action }, [])
	str_list = |_| Param.builder_with_parameter_parser(Many, |values| Ok(values))

	# Control: the shared phantom position written concretely on both
	# builders (must keep working before and after the fix).
	str_list_concrete : {} -> CliBuilder(List(Str), { get_params : {} }, [])
	str_list_concrete = |_| Param.builder_with_parameter_parser(Many, |values| Ok(values))

	# Rest parameter whose `from_action` is a bare type variable, so it can
	# share a chain with tag-row and nominal phantoms.
	str_list_open : {} -> CliBuilder(List(Str), from_action, [])
	str_list_open = |_| Param.builder_with_parameter_parser(Many, |values| Ok(values))
}

ExtractParamValuesParams : {
	args : List(ParsedArg),
	param : [Optional, Many],
}

extract_param_values : ExtractParamValuesParams -> Try({ values : List(Str), remaining_args : List(ParsedArg) }, [ArgErr])
extract_param_values = |{ args, param }|
	match param {
		Optional =>
			match args {
				[] => Ok({ values: [], remaining_args: [] })
				[Parameter(p), .. as rest] => Ok({ values: [p], remaining_args: rest })
			}

		Many =>
			Ok({ values: args.map(|Parameter(p)| p), remaining_args: [] })
	}
