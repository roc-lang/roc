interface InternalTypeId
    exposes [InternalTypeId, fromNat, toNat]
    imports []

InternalTypeId := U32
     has [ Eq, Hash ]

toNat : InternalTypeId -> Nat
toNat = \@InternalTypeId nat -> Num.intCast nat

fromNat : Nat -> InternalTypeId
fromNat = \nat -> @InternalTypeId (Num.intCast nat)
