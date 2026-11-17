BoxyInlineLiteralDestination := {}

# A kept value-form expression in a callback must be frozen at the
# callback's result type, not its narrower branch-local type. The large
# unused error payload puts the result's discriminant at another offset.
Problem : [Bad, Large({ left : List(U64), right : List(U64), value : U64 })]

Label : [NoLabel, Label({ size : U64, width : U64 })]

Block : [TextBlock({ label : [NoLabel, Label(U64)] })]

measure : U64 -> Try({ size : U64, width : U64 }, Problem)
measure = |width| if (width == 0) Err(Bad) else Ok({ size: 12, width })

visit : U64, (U64 -> Try(Label, Problem)) -> Try(U64, Problem)
visit = |count, label_at| {
	var $index = 0
	while $index < count {
		match label_at($index)? {
			Label(_) => {}
			NoLabel => {}
		}
		$index = $index + 1
	}
	Ok($index)
}

count_labels : List(Block) -> Try(U64, Problem)
count_labels = |blocks| visit(
	blocks.len(),
	|index| match blocks.get(index) ?? crash "index" {
		TextBlock({ label: Label(value), .. }) => Ok(Label(measure(value)?))
		_ => Ok(NoLabel)
	},
)

expect count_labels([TextBlock({ label: NoLabel })]) == Ok(1)
expect count_labels([TextBlock({ label: Label(20) }), TextBlock({ label: NoLabel })]) == Ok(2)
expect count_labels([TextBlock({ label: Label(0) })]) == Err(Bad)
