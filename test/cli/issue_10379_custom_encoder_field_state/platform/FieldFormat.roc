import Stdout

FieldFormat :: [].{
	Value : [
		Null,
		Real(F64),
		Integer(I64),
		String(Str),
		Bytes(List(U8)),
	]

	Binding : {
		name : Str,
		value : Value,
	}

	ValueType : [Blob, Integer, Null, Real, Text]

	Error : [
		DuplicateColumn(Str),
		ExpectedSingleColumn({ actual : U64 }),
		InvalidValue({ column : Str }),
		MalformedRow,
		MissingRequiredField(Str),
		MultipleValuesForParameter,
		NestedParameterRecord,
		NoRowsReturned,
		ParameterValueMissing(Str),
		ParameterValueOutsideRecord,
		PoolSaturated,
		QueryTimedOut,
		ResourceSaturated,
		ResultTooLarge({ max_bytes : U64 }),
		RowsReturnedUseQueryInstead,
		SqliteErr(ErrCode, Str),
		TooManyRows({ max_rows : U64 }),
		TooManyRowsReturned,
		ConcurrentTransactionUse,
		TransactionFinished,
		UnconsumedColumns,
		UnexpectedType({ actual : ValueType, column : Str, expected : ValueType }),
	]

	ErrCode : [
		Error,
		Internal,
		Perm,
		Abort,
		Busy,
		Locked,
		NoMem,
		ReadOnly,
		Interrupt,
		IOErr,
		Corrupt,
		NotFound,
		Full,
		CanNotOpen,
		Protocol,
		Empty,
		Schema,
		TooBig,
		Constraint,
		Mismatch,
		Misuse,
		NoLFS,
		AuthDenied,
		Format,
		OutOfRange,
		NotADatabase,
		Notice,
		Warning,
		Row,
		Done,
		Unknown(I64),
	]

	State : {
		bindings : List(Binding),
		field : [Field(Str), NoField],
		value : [Encoded(Value), NoValue],
	}

	Format :: [Default].{
		rename_field : Format, Str -> Str
		rename_field = |_, name| name

		encode_record : State, U64, (State, (State, Str, (State -> Try(State, Error)) -> Try(State, Error)) -> Try(State, Error)) -> Try(State, Error)
		encode_record = |state, _, write_fields|
			match state.field {
				Field(_) => Err(NestedParameterRecord)
				NoField => {
					finished = write_fields(
						state,
						|cursor, name, write_value| {
							encoded = write_value({
								bindings: cursor.bindings,
								field: Field(name),
								value: NoValue,
							})?

							match encoded.value {
								Encoded(value) =>
									Ok({
										bindings: encoded.bindings.append({ name, value }),
										field: NoField,
										value: NoValue,
									})
								NoValue => Err(ParameterValueMissing(name))
							}
						},
					)?
					Ok(finished)
				}
			}

		encode_str : Str, State -> Try(State, Error)
		encode_str = |value, state| set_value(state, String(value))

		encode_i64 : I64, State -> Try(State, Error)
		encode_i64 = |value, state| set_value(state, Integer(value))

		encode_bytes : List(U8), State -> Try(State, Error)
		encode_bytes = |value, state| set_value(state, Bytes(value))
	}

	encode : value -> Try(List(Binding), Error)
		where [
			value.encoder_for : Format -> (value, State -> Try(State, Error)),
		]
	encode = |value| encode_params(value)

	statement : value -> Try(List(Binding), Error)
		where [
			value.encoder_for : Format -> (value, State -> Try(State, Error)),
		]
	statement = |value| encode(value)

	query : value -> Try(List(Binding), Error)
		where [
			value.encoder_for : Format -> (value, State -> Try(State, Error)),
		]
	query = |value| statement(value)

	query! : { params : value } => Try(List(Binding), Error)
		where [
			value.encoder_for : Format -> (value, State -> Try(State, Error)),
		]
	query! = |{ params }| {
		Stdout.line!("preparing")
		statement(params)
	}
}

encode_params : value -> Try(List({ name : Str, value : [Null, Real(F64), Integer(I64), String(Str), Bytes(List(U8))] }), _)
	where [
		value.encoder_for : FieldFormat.Format -> (value, FieldFormat.State -> Try(FieldFormat.State, _)),
	]
encode_params = |value| {
	Shape : value
	encode_value = Shape.encoder_for(FieldFormat.Format.Default)
	encoded = encode_value(
		value,
		{
			bindings: [],
			field: NoField,
			value: NoValue,
		},
	)?
	Ok(encoded.bindings)
}

expect {
	bindings = FieldFormat.encode({ id: 1.I64, name: "two" })?
	bindings.len() == 2
}

Blob := { bytes : List(U8) }.{
	encoder_for : encoding -> (Blob, state -> Try(state, err))
		where [
			encoding.encode_bytes : List(U8), state -> Try(state, err),
		]
	encoder_for = |_encoding| {
		Encoding : encoding
		|blob, state| Encoding.encode_bytes(blob.bytes, state)
	}
}

expect {
	bindings = FieldFormat.encode({ payload: Blob.{ bytes: [1, 2, 3] } })?
	bindings.len() == 1
}

set_value = |state, value|
	match state.field {
		NoField => Err(ParameterValueOutsideRecord)
		Field(_) =>
			match state.value {
				NoValue => Ok({
					bindings: state.bindings,
					field: state.field,
					value: Encoded(value),
				})
				Encoded(_) => Err(MultipleValuesForParameter)
			}
	}
