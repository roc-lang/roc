TypeId :: [TypeId(U64)].{
	from_u64 : U64 -> TypeId
	from_u64 = |id| TypeId(id)

	to_u64 : TypeId -> U64
	to_u64 = |TypeId(id)| id
}
