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
        "roc_answer": answer_for_host,
        "roc_table": table_for_host,
        "roc_names": names_for_host,
        "roc_tree": tree_for_host,
    }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
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
x64mac=4f080b1399350f7efe81079e3994f5846a31158354dcd0a7354fed9a170f17d1
x64win=39a8dd53dff5b39facfa5771954dbfca35640808526960cc879688fb228f507f
x64mingw=39a8dd53dff5b39facfa5771954dbfca35640808526960cc879688fb228f507f
x64freebsd=7fb868f016eae6d67b3b5190ee58cf18d65737df45bcfd0385c184d4b941fcbe
x64openbsd=5c277d06d6a2c994056bda2a987590c9671a50460b195ae140b91cbca8e065fc
x64netbsd=eb84b9868a8ebb65fb4d0899cdfe99f2b7e8892394d441eaba2c18e3c95bdef0
x64musl=eb84b9868a8ebb65fb4d0899cdfe99f2b7e8892394d441eaba2c18e3c95bdef0
x64glibc=eb84b9868a8ebb65fb4d0899cdfe99f2b7e8892394d441eaba2c18e3c95bdef0
x64linux=eb84b9868a8ebb65fb4d0899cdfe99f2b7e8892394d441eaba2c18e3c95bdef0
x64elf=eb84b9868a8ebb65fb4d0899cdfe99f2b7e8892394d441eaba2c18e3c95bdef0
x64v1mac=4f080b1399350f7efe81079e3994f5846a31158354dcd0a7354fed9a170f17d1
x64v1win=39a8dd53dff5b39facfa5771954dbfca35640808526960cc879688fb228f507f
x64v1mingw=39a8dd53dff5b39facfa5771954dbfca35640808526960cc879688fb228f507f
x64v1freebsd=7fb868f016eae6d67b3b5190ee58cf18d65737df45bcfd0385c184d4b941fcbe
x64v1openbsd=5c277d06d6a2c994056bda2a987590c9671a50460b195ae140b91cbca8e065fc
x64v1netbsd=eb84b9868a8ebb65fb4d0899cdfe99f2b7e8892394d441eaba2c18e3c95bdef0
x64v1musl=eb84b9868a8ebb65fb4d0899cdfe99f2b7e8892394d441eaba2c18e3c95bdef0
x64v1glibc=eb84b9868a8ebb65fb4d0899cdfe99f2b7e8892394d441eaba2c18e3c95bdef0
x64v1linux=eb84b9868a8ebb65fb4d0899cdfe99f2b7e8892394d441eaba2c18e3c95bdef0
x64v1elf=eb84b9868a8ebb65fb4d0899cdfe99f2b7e8892394d441eaba2c18e3c95bdef0
arm64mac=eec424e086e9a4000c6efd92d987b8fb584bc6b091e191a0c2a1868908620584
arm64win=53f605e2790cde7dceb3f414294901f9f20ab65174beb4847018f7647b208c9b
arm64mingw=53f605e2790cde7dceb3f414294901f9f20ab65174beb4847018f7647b208c9b
arm64linux=d564b90c3fb5f85b003c56cb9de0df28f7d1357aebf84a9faf70ac5b692589eb
arm64musl=d564b90c3fb5f85b003c56cb9de0df28f7d1357aebf84a9faf70ac5b692589eb
arm64glibc=d564b90c3fb5f85b003c56cb9de0df28f7d1357aebf84a9faf70ac5b692589eb
arm64v1win=53f605e2790cde7dceb3f414294901f9f20ab65174beb4847018f7647b208c9b
arm64v1mingw=53f605e2790cde7dceb3f414294901f9f20ab65174beb4847018f7647b208c9b
arm64v1linux=d564b90c3fb5f85b003c56cb9de0df28f7d1357aebf84a9faf70ac5b692589eb
arm64v1musl=d564b90c3fb5f85b003c56cb9de0df28f7d1357aebf84a9faf70ac5b692589eb
arm64v1glibc=d564b90c3fb5f85b003c56cb9de0df28f7d1357aebf84a9faf70ac5b692589eb
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
