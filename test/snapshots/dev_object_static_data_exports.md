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
x64mac=6853a0ed28679952b0930d6276ee38532ffaf689eeb58deed6a674b81bba006a
x64win=7520d6bce2d2480bd3a521960cc558a67c0812b676234b42bd25f30fd5f0336e
x64freebsd=2673ac590a12ed750de4e59743875ba845d44d664900e514a3e1293b42cc2754
x64openbsd=2673ac590a12ed750de4e59743875ba845d44d664900e514a3e1293b42cc2754
x64netbsd=2673ac590a12ed750de4e59743875ba845d44d664900e514a3e1293b42cc2754
x64musl=2673ac590a12ed750de4e59743875ba845d44d664900e514a3e1293b42cc2754
x64glibc=2673ac590a12ed750de4e59743875ba845d44d664900e514a3e1293b42cc2754
x64linux=2673ac590a12ed750de4e59743875ba845d44d664900e514a3e1293b42cc2754
x64elf=2673ac590a12ed750de4e59743875ba845d44d664900e514a3e1293b42cc2754
arm64mac=47696a2e29c84dc4bbfe120765833dd28a3a1d324e5edd3646492db8f14b7561
arm64win=68fc0695aa9582dac5a39252d6aa7f18a5e508f7ba692874711f9807db53023e
arm64linux=a05d7c335bf54690139a496ec7aaf3c91d98e44ddf7d9828f915f7736d36afcd
arm64musl=a05d7c335bf54690139a496ec7aaf3c91d98e44ddf7d9828f915f7736d36afcd
arm64glibc=a05d7c335bf54690139a496ec7aaf3c91d98e44ddf7d9828f915f7736d36afcd
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
