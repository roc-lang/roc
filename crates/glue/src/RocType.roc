interface RocType
    exposes [
        TypeId,
        RocType,
        Types,
        RocNum,
        RocTagUnion,
        roundUpToAlignment,
        alignment,
        type,
        sizeRoundedToAlignment,
        sizeIgnoringAlignment,
    ]
    imports [Target.{ Target }]

# TODO change this to an opaque type once glue supports abilities.
TypeId : Nat
#      has [
#          Eq {
#              isEq: isEqTypeId,
#          },
#          Hash {
#              hash: hashTypeId,
#          }
#      ]
# isEqTypeId = \@TypeId lhs, @TypeId rhs -> lhs == rhs
# hashTypeId = \hasher, @TypeId id -> Hash.hash hasher id
# TODO: switch AssocList uses to Dict once roc_std is updated.
Types : {
    # These are all indexed by TypeId
    types : List RocType,
    sizes : List U32,
    aligns : List U32,

    # Needed to check for duplicates
    typesByName : List [T Str TypeId],

    ## Dependencies - that is, which type depends on which other type.
    ## This is important for declaration order in C; we need to output a
    ## type declaration earlier in the file than where it gets referenced by another type.
    deps : List [T TypeId (List TypeId)],
    target : Target,
}

RocType : [
    RocStr,
    Bool,
    RocResult TypeId TypeId,
    Num RocNum,
    RocList TypeId,
    RocDict TypeId TypeId,
    RocSet TypeId,
    RocBox TypeId,
    TagUnion RocTagUnion,
    EmptyTagUnion,
    Struct
        {
            name : Str,
            fields : List { name : Str, id : TypeId },
        },
    TagUnionPayload
        {
            name : Str,
            fields : List { discriminant : Nat, id : TypeId },
        },
    ## A recursive pointer, e.g. in StrConsList : [Nil, Cons Str StrConsList],
    ## this would be the field of Cons containing the (recursive) StrConsList type,
    ## and the TypeId is the TypeId of StrConsList itself.
    RecursivePointer TypeId,
    Function
        {
            name : Str,
            args : List TypeId,
            ret : TypeId,
        },
    # A zero-sized type, such as an empty record or a single-tag union with no payload
    Unit,
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
            name : Str,
            tags : List Str,
            size : U32,
        },
    ## A non-recursive tag union
    ## e.g. `Result a e : [Ok a, Err e]`
    NonRecursive
        {
            name : Str,
            tags : List { name : Str, payload : [Some TypeId, None] },
            discriminantSize : U32,
            discriminantOffset : U32,
        },
    ## A recursive tag union (general case)
    ## e.g. `Expr : [Sym Str, Add Expr Expr]`
    Recursive
        {
            name : Str,
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
            name : Str,
            indexOfNullTag : U16,
            tags : List { name : Str, payload : [Some TypeId, None] },
            discriminantSize : U32,
            discriminantOffset : U32,
        },
    ## Optimization: No need to store a tag ID (the payload is "unwrapped")
    ## e.g. `RoseTree a : [Tree a (List (RoseTree a))]`
    NonNullableUnwrapped
        {
            name : Str,
            tagName : Str,
            payload : TypeId, # These always have a payload.
        },
    ## Optimization: No need to store a tag ID (the payload is "unwrapped")
    ## e.g. `[Foo Str Bool]`
    SingleTagStruct
        {
            name : Str,
            tagName : Str,
            payloadFields : List TypeId,
        },
    ## A recursive tag union with only two variants, where one is empty.
    ## Optimizations: Use null for the empty variant AND don't store a tag ID for the other variant.
    ## e.g. `ConsList a : [Nil, Cons a (ConsList a)]`
    NullableUnwrapped
        {
            name : Str,
            nullTag : Str,
            nonNullTag : Str,
            nonNullPayload : TypeId,
            whichTagIsNull : [FirstTagIsNull, SecondTagIsNull],
        },
]

# TODO: to reproduce an Ability bug, uncomment this:
# type : Types, TypeId -> U32
type : _, TypeId -> RocType
type = \types, id -> getOrCrash types .types id

# TODO: to reproduce an Ability bug, uncomment this:
# alignment : Types, TypeId -> U32
alignment : _, TypeId -> U32
alignment = \types, id -> getOrCrash types .aligns id

# TODO: to reproduce an Ability bug, uncomment this:
# sizeIgnoringAlignment : Types, TypeId -> U32
sizeIgnoringAlignment : _, TypeId -> U32
sizeIgnoringAlignment = \types, id -> getOrCrash types .sizes id

# TODO: to reproduce an Ability bug, uncomment this:
# sizeRoundedToAlignment : Types, TypeId -> U32
sizeRoundedToAlignment : _, TypeId -> U32
sizeRoundedToAlignment = \types, id ->
    sizeIgnoringAlignment types id
    |> roundUpToAlignment (alignment types id)

roundUpToAlignment : U32, U32 -> U32
roundUpToAlignment = \width, align ->
    when align is
        0 -> width
        1 -> width
        _ ->
            if width % align > 0 then
                width + align - (width % align)
            else
                width

getOrCrash : _, _, TypeId -> _
getOrCrash = \types, typesToList, id ->
    when List.get (typesToList types) id is
        Ok answer -> answer
        Err OutOfBounds ->
            idStr = Num.toStr id
            crash "TypeId \(idStr) was not found in Types. This should never happen!"
