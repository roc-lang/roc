interface Set
    exposes 
        [ 
            empty,
            single,
            walk,
            insert,
            len,
            remove,
            contains,
            toList,
            fromList,
            union,
            intersection,
            difference,
        ]
    imports [ List, Bool.{ Bool } ]

empty : Set k
single : k -> Set k
insert : Set k, k -> Set k
len : Set k -> Nat
remove : Set k, k -> Set k
contains : Set k, k -> Bool

# toList = \set -> Dict.keys (toDict set)
toList : Set k -> List k
fromList : List k -> Set k

union : Set k, Set k -> Set k
intersection : Set k, Set k -> Set k
difference : Set k, Set k -> Set k

toDict : Set k -> Dict k {}

walk : Set k, state, (state, k -> state) -> state
walk = \set, state, step ->
    Dict.walk (toDict set) state (\s, k, _ -> step s k)
