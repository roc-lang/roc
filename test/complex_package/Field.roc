## Field module for defining and validating record-like data.
## Imports both Validator and Util, forming a diamond dependency through Util.
##
## Dependency: Field → Validator → Util
##             Field → Util (direct)

import Validator
import Util

Field := {
	name : Str,
	value : Str,
	is_required : Bool,
	validators : List(Validator.Validator),
}.{

	## Mark a field as required and add a not-empty validator.
	required : Field -> Field
	required = |field|
		{
			..field,
			is_required: Bool.True,
			validators: field.validators.append(Validator.not_empty(field.name)),
		}

	## Add a min-length validator to a field.
	with_min_length : Field, U64 -> Field
	with_min_length = |field, min|
		{
			..field,
			validators: field.validators.append(Validator.min_length(field.name, min)),
		}

	## Validate a single field, returning Ok(value) or Err with combined error messages.
	validate : Field -> Try(Str, Str)
	validate = |field| {
		cleaned = Util.trim_all([field.value])
		val = match cleaned {
			[v] => v
			_ => ""
		}

		match Validator.run_all(field.validators, val) {
			Ok(v) => Ok(v)
			Err(errors) => Err(Util.join_with(errors, "; "))
		}
	}

	## Validate a list of fields, returning a list of (name, result) pairs.
	validate_all : List(Field) -> List({ name : Str, result : Try(Str, Str) })
	validate_all = |fields|
		fields.map(
			|f|
				{ name: f.name, result: validate(f) },
		)
}

# Tests

expect {
	f = Field.required({ name: "email", value: "test@example.com", is_required: Bool.False, validators: [] })
	Field.validate(f) == Ok("test@example.com")
}

expect {
	f = Field.required({ name: "email", value: "  ", is_required: Bool.False, validators: [] })
	Field.validate(f) == Err("email must not be empty")
}

expect {
	f = { name: "note", value: "", is_required: Bool.False, validators: [] }
	Field.validate(f) == Ok("")
}

expect {
	f1 = Field.required({ name: "name", value: "Alice", is_required: Bool.False, validators: [] })
	f2 = Field.required({ name: "email", value: "", is_required: Bool.False, validators: [] })
	r1 = Field.validate(f1)
	r2 = Field.validate(f2)
	r1 == Ok("Alice") and r2 == Err("email must not be empty")
}
