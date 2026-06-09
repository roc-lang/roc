# META
~~~ini
description=Provided non-function constants become readonly object data symbols
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [answer, table, names, tree] { pf: platform "./platform.roc" }

Tree : [Leaf(I64), Node(Box(Branch), Box(Branch))]
Branch : [BranchLeaf(I64), BranchPair(Box(I64), Box(I64))]

answer : I64
answer = 42

table : {
    user: {
        name: Str,
        tags: List(Str),
    },
    counts: (I64, I64),
    status: [Ok(Str), Err(Str)],
}
table = {
    user: {
        name: "Alice",
        tags: ["admin", "ops"],
    },
    counts: (3, 5),
    status: Ok("ready"),
}

names : List(List(Str))
names = [["Alice", "Bob"], [], ["Eve"]]

tree : Tree
tree =
    Node(
        Box.box(BranchLeaf(5)),
        Box.box(BranchPair(
            Box.box(7),
            Box.box(11),
        )),
    )
~~~
## platform.roc
~~~roc
platform ""
    requires {} {
        answer : I64,
        table : {
            user: {
                name: Str,
                tags: List(Str),
            },
            counts: (I64, I64),
            status: [Ok(Str), Err(Str)],
        },
        names : List(List(Str)),
        tree : [
            Leaf(I64),
            Node(
                Box([BranchLeaf(I64), BranchPair(Box(I64), Box(I64))]),
                Box([BranchLeaf(I64), BranchPair(Box(I64), Box(I64))]),
            ),
        ],
    }
    exposes []
    packages {}
    provides {
        answer_for_host: "answer",
        table_for_host: "table",
        names_for_host: "names",
        tree_for_host: "tree",
    }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: { files: [app] },
        }
    }

answer_for_host : I64
answer_for_host = answer

table_for_host : {
    user: {
        name: Str,
        tags: List(Str),
    },
    counts: (I64, I64),
    status: [Ok(Str), Err(Str)],
}
table_for_host = table

names_for_host : List(List(Str))
names_for_host = names

tree_for_host : [
    Leaf(I64),
    Node(
        Box([BranchLeaf(I64), BranchPair(Box(I64), Box(I64))]),
        Box([BranchLeaf(I64), BranchPair(Box(I64), Box(I64))]),
    ),
]
tree_for_host = tree
~~~
# MONO
~~~roc
# platform
answer_for_host = <required>
table_for_host = <required>
names_for_host = <required>
tree_for_host = <required>

# app
answer = 42
table = { user: { name: "Alice", tags: ["admin", "ops"] }, counts: (3, 5), status: Ok("ready") }
names = [["Alice", "Bob"], [], ["Eve"]]
tree = Node(box(BranchLeaf(5)), box(BranchPair(box(7), box(11))))

~~~
# DEV OUTPUT
~~~ini
x64mac=4fa5a46c6198037b9f894d6ff12bd57299ac059d723eb08d0a41857b56bfbd13
x64win=b94ee67251b8b99f8fea5ad49511bc600694298d186e8b73ab67167d8cc28a49
x64freebsd=f2608279e68561f780d860b1326338f791a492f2281b95ca2be0f92707e16de7
x64openbsd=f2608279e68561f780d860b1326338f791a492f2281b95ca2be0f92707e16de7
x64netbsd=f2608279e68561f780d860b1326338f791a492f2281b95ca2be0f92707e16de7
x64musl=f2608279e68561f780d860b1326338f791a492f2281b95ca2be0f92707e16de7
x64glibc=f2608279e68561f780d860b1326338f791a492f2281b95ca2be0f92707e16de7
x64linux=f2608279e68561f780d860b1326338f791a492f2281b95ca2be0f92707e16de7
x64elf=f2608279e68561f780d860b1326338f791a492f2281b95ca2be0f92707e16de7
arm64mac=dba7ab6aa0f3ff19e21152aa0fc25d87ba9ecc7d6d852d75ec18e21dff5d75fb
arm64win=349b3649e6345b9a39814f7fa28d3b1854bfa99430c99dab5fbf8c8a5f1654e6
arm64linux=ae04073d9a6611affe9725d5eacb583fa3c687abc7de7b08a76200c89d14c0c5
arm64musl=ae04073d9a6611affe9725d5eacb583fa3c687abc7de7b08a76200c89d14c0c5
arm64glibc=ae04073d9a6611affe9725d5eacb583fa3c687abc7de7b08a76200c89d14c0c5
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
