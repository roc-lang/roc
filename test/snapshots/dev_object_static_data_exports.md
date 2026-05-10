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
            x64glibc: [app],
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
x64mac=1550ef219623e70f9c9583017fa303eb3212696a1cf50c964e29935cbc84a20f
x64win=2dd05be704580bb6795d3070f7e5d3be90af579221df3cb667262ebc590b0ff7
x64freebsd=2d7527fbb575bb77fa7fb8e4b91a17db4784b1e2521ac2c8c4c9b81bf0cf4cb2
x64openbsd=2d7527fbb575bb77fa7fb8e4b91a17db4784b1e2521ac2c8c4c9b81bf0cf4cb2
x64netbsd=2d7527fbb575bb77fa7fb8e4b91a17db4784b1e2521ac2c8c4c9b81bf0cf4cb2
x64musl=2d7527fbb575bb77fa7fb8e4b91a17db4784b1e2521ac2c8c4c9b81bf0cf4cb2
x64glibc=2d7527fbb575bb77fa7fb8e4b91a17db4784b1e2521ac2c8c4c9b81bf0cf4cb2
x64linux=2d7527fbb575bb77fa7fb8e4b91a17db4784b1e2521ac2c8c4c9b81bf0cf4cb2
x64elf=2d7527fbb575bb77fa7fb8e4b91a17db4784b1e2521ac2c8c4c9b81bf0cf4cb2
arm64mac=f7e383110334b40efef54040211349267d6bf1715042b00d75028eba65c21b3c
arm64win=7a04e3e2be3bf4d6808408d0faacff8e67e6981e79a02f2ab04c858476336488
arm64linux=e6d482cd40641cb18aa88e29a29295157ac8e550413fe2e78db0cbcb4183da3a
arm64musl=e6d482cd40641cb18aa88e29a29295157ac8e550413fe2e78db0cbcb4183da3a
arm64glibc=e6d482cd40641cb18aa88e29a29295157ac8e550413fe2e78db0cbcb4183da3a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
