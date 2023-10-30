interface Type
    exposes [Type, RocNum, RocTagUnion, Fields, RocFn, SingleTagPayload]
    imports [TypeId.{ TypeId }]

Type : [
    Str,
    Bool,
    Result TypeId TypeId,
    Num RocNum,
    List TypeId,
    Dict TypeId TypeId,
    Set TypeId,
    Box TypeId,
    TagUnion RocTagUnion,
    EmptyTagUnion,
    Record Fields,
    Tuple Fields,
    TagUnionPayload Fields,
    TypeAlias Str TypeId,
    ## A recursive pointer, e.g. in StrConsList : [Nil, Cons Str StrConsList],
    ## this would be the field of Cons containing the (recursive) StrConsList type,
    ## and the TypeId is the TypeId of StrConsList itself.
    RecursivePointer TypeId,
    Function RocFn,
    # A zero-sized type, such as an empty record or a single-tag union with no payload
    Unit,
    Unsized,
]

RocNum : [
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    I128,
    U128,
    F32,
    F64,
    Dec,
]

RocTagUnion : [
    Enumeration
        {
            tags : List Str,
            size : U32,
        },
    ## A non-recursive tag union
    ## e.g. `Result a e : [Ok a, Err e]`
    NonRecursive
        {
            tags : List { name : Str, payload : [Some TypeId, None] },
            discriminantSize : U32,
            discriminantOffset : U32,
        },
    ## A recursive tag union (general case)
    ## e.g. `Expr : [Sym Str, Add Expr Expr]`
    Recursive
        {
            tags : List { name : Str, payload : [Some TypeId, None] },
            discriminantSize : U32,
            discriminantOffset : U32,
        },
    ## A recursive tag union that has an empty variant
    ## Optimization: Represent the empty variant as null pointer => no memory usage & fast comparison
    ## It has more than one other variant, so they need tag IDs (payloads are "wrapped")
    ## e.g. `FingerTree a : [Empty, Single a, More (Some a) (FingerTree (Tuple a)) (Some a)]`
    ## see also: https://youtu.be/ip92VMpf_-A?t=164
    NullableWrapped
        {
            indexOfNullTag : U16,
            tags : List { name : Str, payload : [Some TypeId, None] },
            discriminantSize : U32,
            discriminantOffset : U32,
        },
    ## Optimization: No need to store a tag ID (the payload is "unwrapped")
    ## e.g. `RoseTree a : [Tree a (List (RoseTree a))]`
    NonNullableUnwrapped
        {
            tagName : Str,
            payload : TypeId, # These always have a payload.
        },
    ## Optimization: No need to store a tag ID (the payload is "unwrapped")
    ## e.g. `[Foo Str Bool]`
    SingleTagStruct
        {
            tagName : Str,
            payload : SingleTagPayload,
        },
    ## A recursive tag union with only two variants, where one is empty.
    ## Optimizations: Use null for the empty variant AND don't store a tag ID for the other variant.
    ## e.g. `ConsList a : [Nil, Cons a (ConsList a)]`
    NullableUnwrapped
        {
            nullTag : Str,
            nonNullTag : Str,
            nonNullPayload : TypeId,
            whichTagIsNull : [FirstTagIsNull, SecondTagIsNull],
        },
]

Fields : [
    HasNoClosure (List { name : Str, id : TypeId }),
    HasClosure (List { name : Str, id : TypeId, accessors : { getter : Str } }),
]

SingleTagPayload : [
    HasClosure (List { name : Str, id : TypeId }),
    HasNoClosure (List { id : TypeId }),
]

RocFn : {
    functionName : Str,
    externName : Str,
    args : List TypeId,
    lambdaSet : TypeId,
    ret : TypeId,
    isToplevel : Bool,
}
