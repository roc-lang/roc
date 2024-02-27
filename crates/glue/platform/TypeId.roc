interface TypeId
    exposes [TypeId, fromU64, toU64]
    imports []

TypeId := U64 implements [Eq, Hash]

toU64 : TypeId -> U64
toU64 = \@TypeId x -> x

fromU64 : U64 -> TypeId
fromU64 = @TypeId
