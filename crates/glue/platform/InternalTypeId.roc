interface InternalTypeId
    exposes [InternalTypeId, fromU64, toU64]
    imports []

InternalTypeId : U64

toU64 := InternalTypeId -> U64
toU64 = \@InternalTypeId x -> x

fromU64 : U64 -> InternalTypeId
fromU64 = \x -> @InternalTypeId x
