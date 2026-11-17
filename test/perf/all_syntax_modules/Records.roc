import Builder
import Shared

Records := [].{
	tuple_demo = ("Roc", 1)

	type_var : List(a) -> List(a)
	type_var = |lst| lst

	destructuring = || {
		tup : (Str, Dec)
		tup = ("Roc", 1.0)
		(str, num) = tup

		rec : { x : Dec, y : Dec }
		rec = { x: 1.0, y: tup.1 }
		{ x, y } = rec

		(str, num, x, y)
	}

	record_builder = || {
		built = {
			count: Builder.pure(42.U64),
			name: Builder.pure("Roc"),
		}.Builder
		built.run()
	}

	record_update : { name : Str, age : I64 } -> { name : Str, age : I64 }
	record_update = |person| {
		{ ..person, age: 31 }
	}

	remove_record_field : { name : Str, age : I64, email : Str } -> { name : Str, age : I64 }
	remove_record_field = |person| {
		{ email: _, ..rest } = person
		rest
	}

	number_literals = {
		usage_based: 5,
		explicit_u8: 5.U8,
		explicit_i8: 5.I8,
		explicit_u16: 5.U16,
		explicit_i16: 5.I16,
		explicit_u32: 5.U32,
		explicit_i32: 5.I32,
		explicit_u64: 5.U64,
		explicit_i64: 5.I64,
		explicit_u128: 5.U128,
		explicit_i128: 5.I128,
		hex: 0x5,
		octal: 0o5,
		binary: 0b0101,
		label: Shared.label("literals", "all"),
	}
}
