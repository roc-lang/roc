import PoolTables

PoolOther :: {}.{
	other : U64 -> U64
	other = |n| PoolTables.shared_table.get(n + 1) ?? 0
}
