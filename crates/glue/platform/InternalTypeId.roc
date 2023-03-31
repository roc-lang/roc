interface InternalTypeId
    exposes [InternalTypeId, fromNat, toNat]
    imports []

InternalTypeId := Nat
     has [ Eq { isEq }, Hash { hash } ]

isEq : InternalTypeId, InternalTypeId -> Bool
isEq = \@InternalTypeId lhs, @InternalTypeId rhs -> lhs == rhs

hash : hasher, InternalTypeId -> hasher | hasher has Hasher
hash = \hasher, @InternalTypeId id -> Hash.hash hasher id

toNat : InternalTypeId -> Nat
toNat = \@InternalTypeId nat -> nat

fromNat : Nat -> InternalTypeId
fromNat = @InternalTypeId
