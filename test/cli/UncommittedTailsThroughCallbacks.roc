UncommittedTailsThroughCallbacks :: [].{}

# A record literal a function hands to a callback, or returns from a function
# it returns, leaves the function, so its uncommitted tail stays quantified and
# each use can lift it into a nominal type. `Row` has an unnamed field and an
# optional one, so the lift changes the record's layout on its way out.

Row := { columns : List(Str), _ : U32, values : List(Str), note ?: Str }

describe : Row -> Str
describe = |row| {
	note = match row.?note {
		Ok(text) => text
		Err(MissingField) => "-"
	}
	Str.concat(Str.join_with(row.values, ","), note)
}

decode_rows = |columns, raw_rows, decode_row| List.map(raw_rows, |values| decode_row({ columns, values }))

make_reader = |columns| |values| { columns, values }

expect {
	rows : List(Row)
	rows = decode_rows(["a"], [["1"], ["2"]], |row| row)
	List.map(rows, describe) == ["1-", "2-"]
}

expect {
	reader : List(Str) -> Row
	reader = make_reader(["a"])
	describe(reader(["x", "y"])) == "x,y-"
}

expect decode_rows(["a"], [["3"]], |row| Str.join_with(row.columns, ",")) == ["a"]

expect make_reader(["b"])(["z"]).columns == ["b"]
