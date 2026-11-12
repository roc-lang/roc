OpenRecordsHintFixes :: [].{}

# Each fix the open-record refusal hints suggest, applied without a type
# annotation, and the annotation each hint also mentions.

Row := { columns : U64, _ : U32, values : U64 }

Wide := { a : U8, note ?: Str }

Cfg := { a : U8, level : U8 ?? 3 }

# Padded, record built: build it with `Row.{ ... }`.
make_row = |columns, values| Row.{ columns, values }

# Padded, record taken apart: take it apart with `Row.{ ... }`.
product = |Row.{ columns, values }| columns * values

# Added field, record built, nominal: build it with `Wide.{ ... }`.
make_wide = |a| Wide.{ a: a }

# Added field, record built, structural: add the field where it's built.
make_noted = |a| { a, note: "n" }

# Added field, record taken apart, nominal: `Name.{ ..., .. }`, for an
# optional and for a defaulted field.
get_a = |Wide.{ a, .. }| a

get_level = |Cfg.{ level, .. }| level

# The annotations the hints mention.
annotated_row : U64, U64 -> Row
annotated_row = |columns, values| { columns, values }

annotated_product : Row -> U64
annotated_product = |{ columns, values }| columns * values

annotated_wide : U8 -> Wide
annotated_wide = |a| { a: a }

row : Row
row = { columns: 3, values: 4 }

wide : Wide
wide = { a: 1 }

cfg : Cfg
cfg = { a: 2 }

expect product(make_row(2, 5)) == 10

expect product(row) == 12

expect get_a(make_wide(5)) == 5

expect get_a(wide) == 1

expect get_level(cfg) == 3

expect {
	w : { a : U8, note ?: Str }
	w = make_noted(3)
	w.a == 3
}

expect {
	w : Wide
	w = make_noted(4)
	w.a == 4
}

expect annotated_product(annotated_row(6, 7)) == 42

expect get_a(annotated_wide(8)) == 8
