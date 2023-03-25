interface InternalTypeId
    exposes [InternalTypeId, fromNat, toNat]
    imports []

InternalTypeId := Nat
     has [ Eq, Hash ]

toNat : InternalTypeId -> Nat
toNat = \@InternalTypeId nat -> nat

fromNat : Nat -> InternalTypeId
fromNat = @InternalTypeId
