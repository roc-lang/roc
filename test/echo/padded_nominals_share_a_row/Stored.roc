## Functions returning records this module never relates to a nominal. Each
## call in another module copies the result's type, so a call can lift the
## records into a padded nominal this module does not name.
Stored := {}.{
	rows = |_| [{ x: 1.U8, y: 2.U64 }, { x: 3.U8, y: 4.U64 }]
	nested = |_| [[{ x: 5.U8, y: 6.U64 }], []]
	empty_rows = |_| List.drop_first([{ x: 0.U8, y: 0.U64 }], 1)
	long = |_| [{ s: "a stored string long enough to be on the heap", y: 7.U64 }]
	units = |_| [{}, {}]
	tagged = |_| [Has({ x: 8.U8, y: 9.U64 }), No]
	tupled = |_| ([{ x: 10.U8, y: 11.U64 }], 12.U8)
	boxed = |_| Box.box({ x: 13.U8, y: 14.U64 })
	boxes = |_| [Box.box({ x: 15.U8, y: 16.U64 })]
	to_rows = |n| [{ x: n, y: 17.U64 }]
}
