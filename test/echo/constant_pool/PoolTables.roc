PoolTables :: {}.{
	# Each table's first two items are a byte pattern no other data holds, so
	# a test can count the copies of the table a binary carries.
	uncalled_table : List(U64)
	uncalled_table = List.repeat(0, 4).map_with_index(|_, i| 0x51A7_0000_0000_0A00 + i)

	shared_table : List(U64)
	shared_table = List.repeat(0, 4).map_with_index(|_, i| 0x51A7_0000_0000_0B00 + i)

	method_table : List(U64)
	method_table = List.repeat(0, 4).map_with_index(|_, i| 0x51A7_0000_0000_0C00 + i)

	# Nothing calls this, so nothing keeps `uncalled_table`.
	uncalled : U64 -> U64
	uncalled = |n| uncalled_table.get(n) ?? 0

	shared : U64 -> U64
	shared = |n| shared_table.get(n) ?? 0

	Probe := { n : U64 }.{
		value = |p| method_table.get(p.n) ?? 0
	}

	probe : U64 -> Probe
	probe = |n| Probe.({ n: n })
}
