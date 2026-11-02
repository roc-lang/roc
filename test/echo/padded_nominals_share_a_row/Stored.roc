## Values of record rows this module never relates to a nominal. Their types are
## fixed here, so a read in another module that lifts one into a padded nominal
## has to convert it.
Stored := {}.{
	rows = [{ x: 1.U8, y: 2.U64 }, { x: 3.U8, y: 4.U64 }]
	nested = [[{ x: 5.U8, y: 6.U64 }], []]
	empty_rows : List({ x : U8, y : U64 })
	empty_rows = []
	long = [{ s: "a stored string long enough to be on the heap", y: 7.U64 }]
	units = [{}, {}]
	tagged = [Has({ x: 8.U8, y: 9.U64 }), No]
	tupled = ([{ x: 10.U8, y: 11.U64 }], 12.U8)
	boxed = Box.box({ x: 13.U8, y: 14.U64 })
	boxes = [Box.box({ x: 15.U8, y: 16.U64 })]
	to_rows = |n| [{ x: n, y: 17.U64 }]
}
