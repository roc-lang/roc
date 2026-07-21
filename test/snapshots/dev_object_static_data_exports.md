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
x64mac=ac961554b3ff6db4f09679bde32c376f3c0f379cf4a67f1af7443eb9c2425389
x64win=06c44523b19e355d569e4a899fe8712476967416abf85d6fdda816a2558ade8d
x64freebsd=ad6c589bcc21eeef5f64e3a29157c7f2b210eb55cf6a6a8973bc6faf77d120ad
x64openbsd=ad6c589bcc21eeef5f64e3a29157c7f2b210eb55cf6a6a8973bc6faf77d120ad
x64netbsd=ad6c589bcc21eeef5f64e3a29157c7f2b210eb55cf6a6a8973bc6faf77d120ad
x64musl=ad6c589bcc21eeef5f64e3a29157c7f2b210eb55cf6a6a8973bc6faf77d120ad
x64glibc=ad6c589bcc21eeef5f64e3a29157c7f2b210eb55cf6a6a8973bc6faf77d120ad
x64linux=ad6c589bcc21eeef5f64e3a29157c7f2b210eb55cf6a6a8973bc6faf77d120ad
x64elf=ad6c589bcc21eeef5f64e3a29157c7f2b210eb55cf6a6a8973bc6faf77d120ad
arm64mac=776fa06e8a379d3a6d48420edb2aeaaecf22a1be9f1e77e69af1bb8918aec1c1
arm64win=3cbb4aaefebfbceed61d1edd84aa694aed8d265f6f776e4ec68bb5134e0f32df
arm64linux=7cd5d48d482155eb8ded7b1b68027e81f3aec89a0c5015e1bad79ef80726b647
arm64musl=7cd5d48d482155eb8ded7b1b68027e81f3aec89a0c5015e1bad79ef80726b647
arm64glibc=7cd5d48d482155eb8ded7b1b68027e81f3aec89a0c5015e1bad79ef80726b647
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
