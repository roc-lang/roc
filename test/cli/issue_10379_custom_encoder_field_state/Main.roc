app [main!] { pf: platform "./platform/main.roc" }

import pf.FieldFormat
import pf.Stdout

Status := [Active, Inactive].{
	encoder_for : encoding -> (Status, state -> Try(state, err))
		where [
			encoding.encode_str : Str, state -> Try(state, err),
		]
	encoder_for = |_encoding| {
		Encoding : encoding

		|status, state|
			Encoding.encode_str(
				match status {
					Active => "active"
					Inactive => "inactive"
				},
				state,
			)
	}
}

expect {
	value : { status : Status }
	value = { status: Active }
	bindings = FieldFormat.query(value)?
	binding = bindings.get(0)?

	name_ok = binding.name == "status"
	value_ok = match binding.value {
		String(text) => text == "active"
		_ => False
	}

	name_ok and value_ok
}

main! = || {
	status : Status
	status = Active
	bindings = match FieldFormat.query!({
		params: { status },
	}) {
		Ok(encoded) => encoded
		Err(_) => {
			crash "encoding failed"
		}
	}
	binding = match bindings {
		[first, ..] => first
		_ => {
			crash "encoding returned no bindings"
		}
	}

	if binding.name == "status" and match binding.value {
		String(text) => text == "active"
		_ => False
	} {
		Stdout.line!("passed")
	} else {
		crash "custom encoder received the wrong field state"
	}
}
