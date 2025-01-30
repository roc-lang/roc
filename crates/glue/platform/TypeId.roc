module [TypeId, type_id_from_u64, type_id_to_u64]

TypeId := U64 implements [Eq, Hash, Inspect, Encoding]

# renamed here so we can import the functions directly as a workaround for
# https://github.com/roc-lang/roc/issues/5477
type_id_to_u64 : TypeId -> U64
type_id_to_u64 = \@TypeId(x) -> x

type_id_from_u64 : U64 -> TypeId
type_id_from_u64 = @TypeId
