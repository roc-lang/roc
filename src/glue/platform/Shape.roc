TypeId := U64

Shape : [
    RocStr,
    Bool,
    RocResult(TypeId, TypeId),
    Num(RocNum),
    RocList(TypeId),
    RocDict(TypeId, TypeId),
    RocSet(TypeId),
    RocBox(TypeId),
    TagUnion(RocTagUnion),
    EmptyTagUnion,
    Struct(
        {
            name : Str,
            fields : RocStructFields,
        }),
    TagUnionPayload(
        {
            name : Str,
            fields : RocStructFields,
        }),
    ## A recursive pointer, e.g. in StrConsList : [Nil, Cons Str StrConsList],
    ## this would be the field of Cons containing the (recursive) StrConsList type,
    ## and the TypeId is the TypeId of StrConsList itself.
    RecursivePointer(TypeId),
    Function(RocFn),
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
    Enumeration(
        {
            name : Str,
            tags : List(Str),
            size : U32,
        }),
    ## A non-recursive tag union
    ## e.g. `Result a e : [Ok a, Err e]`
    NonRecursive(
        {
            name : Str,
            tags : List({ name : Str, payload : [Some(TypeId), None] }),
            discriminant_size : U32,
            discriminant_offset : U32,
        }),
    ## A recursive tag union (general case)
    ## e.g. `Expr : [Sym Str, Add Expr Expr]`
    Recursive(
        {
            name : Str,
            tags : List({ name : Str, payload : [Some(TypeId), None] }),
            discriminant_size : U32,
            discriminant_offset : U32,
        }),
    ## A recursive tag union that has an empty variant
    ## Optimization: Represent the empty variant as null pointer => no memory usage & fast comparison
    ## It has more than one other variant, so they need tag IDs (payloads are "wrapped")
    NullableWrapped(
        {
            name : Str,
            index_of_null_tag : U16,
            tags : List({ name : Str, payload : [Some(TypeId), None] }),
            discriminant_size : U32,
            discriminant_offset : U32,
        }),
    ## Optimization: No need to store a tag ID (the payload is "unwrapped")
    NonNullableUnwrapped(
        {
            name : Str,
            tag_name : Str,
            payload : TypeId, # These always have a payload.
        }),
    ## Optimization: No need to store a tag ID (the payload is "unwrapped")
    SingleTagStruct(
        {
            name : Str,
            tag_name : Str,
            payload : RocSingleTagPayload,
        }),
    ## A recursive tag union with only two variants, where one is empty.
    ## Optimizations: Use null for the empty variant AND don't store a tag ID for the other variant.
    NullableUnwrapped(
        {
            name : Str,
            null_tag : Str,
            non_null_tag : Str,
            non_null_payload : TypeId,
            which_tag_is_null : [FirstTagIsNull, SecondTagIsNull],
        }),
]

RocStructFields : [
    HasNoClosure(List({ name : Str, id : TypeId })),
    HasClosure(List({ name : Str, id : TypeId, accessors : { getter : Str } })),
]

RocSingleTagPayload : [
    HasClosure(List({ name : Str, id : TypeId })),
    HasNoClosure(List({ id : TypeId })),
]

RocFn : {
    function_name : Str,
    extern_name : Str,
    args : List(TypeId),
    lambda_set : TypeId,
    ret : TypeId,
    is_toplevel : Bool,
}
