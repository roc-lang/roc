app [main!, answer, flag, flags, table, names, tree, boxed_add_one] { pf: platform "./platform/main.roc" }

Branch : [BranchLeaf(I64), BranchPair(Box(I64), Box(I64))]
Tree : [Leaf(I64), Node(Box(Branch), Box(Branch))]

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

boxed_add_one : Box(I64 -> I64)
boxed_add_one = Box.box(|value| value + 1)
