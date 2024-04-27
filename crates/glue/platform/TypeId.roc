interface TypeId
    exposes [TypeId, typeIDfromU64, typeIDtoU64]
    imports []

TypeId := U64 implements [Eq, Hash, Inspect, Encoding]

# renamed here so we can import the functions directly as a workaround for
# https://github.com/roc-lang/roc/issues/5477
typeIDtoU64 : TypeId -> U64
typeIDtoU64 = \@TypeId x -> x

typeIDfromU64 : U64 -> TypeId
typeIDfromU64 = @TypeId
