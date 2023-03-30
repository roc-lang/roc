interface InternalTypeId
    exposes [InternalTypeId, fromNat, toNat]
    imports []

InternalTypeId : Nat

toNat : InternalTypeId -> Nat
toNat = \x -> x

fromNat : Nat -> InternalTypeId
fromNat = \x -> x
