interface RocType
    exposes [RocType, RocNum, TypeId, Types, Target, Architecture, OperatingSystem]
    imports [Target.{ Target, Architecture, OperatingSystem }]

# TODO move into separate Target.roc interface once glue works across interfaces.
Target : {
    architecture: Architecture,
    operating_system: OperatingSystem,
}

Architecture : [
    Aarch32,
    Aarch64,
    Wasm32,
    X86_32,
    X86_64,
]

OperatingSystem : [
    Windows,
    Unix,
    Wasi,
]

TypeId := Nat

Types := {
    # These are all indexed by TypeId
    types: List RocType,
    sizes: List U32,
    aligns: List U32,

    # Needed to check for duplicates
    types_by_name: Dict Str TypeId,

    ## Dependencies - that is, which type depends on which other type.
    ## This is important for declaration order in C; we need to output a
    ## type declaration earlier in the file than where it gets referenced by another type.
    deps: Dict TypeId (List TypeId),
    target: Target,
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
    Struct {
        name: Str,
        fields: List { name: Str, type: TypeId }
    },
    TagUnionPayload {
        name: String,
        fields: List { discriminant: Nat, type: TypeId },
    },
    ## A recursive pointer, e.g. in StrConsList : [Nil, Cons Str StrConsList],
    ## this would be the field of Cons containing the (recursive) StrConsList type,
    ## and the TypeId is the TypeId of StrConsList itself.
    RecursivePointer TypeId,
    Function {
        name: Str,
        args: List TypeId,
        ret: TypeId,
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
    F128,
    Dec,
]