Catalog := [].{
    Payload := {
        id : U64,
        name : Str,
        flags : List(Bool),
    }

    PayloadAlias : Payload

    Point := {
        x : I32,
        y : I32,
    }

    Tree := [
        Leaf(Str),
        Node(Box(Tree), Box(Tree)),
    ]

    SinglePayload : [Only(Payload)]
    SingleNoPayload : [Ready]
    MultiPayload : [Stepped(U64, List(U64))]

    ZeroSizedWrapper := {
        empty : {},
        only : F64,
    }

    CatalogUnion : [
        Payload(Payload),
        Pair(Point, Point),
        Recursive(Box(Tree)),
        Empty,
    ]
}
