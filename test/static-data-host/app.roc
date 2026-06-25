app [
    main!,
    answer,
    flag,
    flags,
    table,
    names,
    tree,
    boxed_add_one,
    literal_long,
    assembled_strings,
    intermediate_final,
    static_slices,
] { pf: platform "./platform/main.roc" }

Branch : [BranchLeaf(I64), BranchPair(Box(I64), Box(I64))]
Tree : [Leaf(I64), Node(Box(Branch), Box(Branch))]
I64ToI64 : I64 -> I64

main! = || {}

answer : I64
answer = 42

flag : Bool
flag = True

flags : List(Bool)
flags = [False, True, False]

table : {
    counts: (I64, I64),
    status: [Err(Str), Ok(Str)],
    user: {
        name: Str,
        tags: List(Str),
    },
}
table = {
    counts: (3, 5),
    status: Ok("ready readonly exported status"),
    user: {
        name: "Alice readonly exported name",
        tags: [
            "admin readonly exported tag",
            "ops readonly exported tag",
        ],
    },
}

names : List(List(Str))
names = [
    [
        "Alice readonly nested list",
        "Bob readonly nested list",
    ],
    [],
    ["Eve readonly nested list"],
]

tree : Tree
tree =
    Node(
        Box.box(BranchLeaf(5)),
        Box.box(BranchPair(
            Box.box(7),
            Box.box(11),
        )),
    )

boxed_add_one : Box(I64ToI64)
boxed_add_one = Box.box(|value| value + 1)

literal_long : Str
literal_long = "literal readonly string longer than thirty bytes"

assembled_strings : (Str, Str, Str)
assembled_strings = {
    first = "assembled readonly first ".concat("string from comptime concat")
    second = "assembled readonly second ".concat("string from comptime concat")
    third = first.concat(" + ").concat(second)
    (first, second, third)
}

intermediate_final : Str
intermediate_final = {
    intermediate = "INTERMEDIATE_ONLY_LEFT_SHOULD_NOT_BE_EMITTED".concat("_INTERMEDIATE_ONLY_RIGHT_SHOULD_NOT_BE_EMITTED")
    empty_suffix = intermediate.drop_prefix(intermediate)
    "final readonly string after comptime branch".concat(empty_suffix)
}

static_slices : (Str, Str)
static_slices = {
    source = "STATIC_SLICE_SOURCE:abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    tail = source.drop_prefix("STATIC_SLICE_SOURCE:")
    (source, tail)
}
