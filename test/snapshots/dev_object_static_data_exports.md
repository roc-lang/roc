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
x64mac=80acd34f0a04bcc8ed2aa7aa4a3e78de612cf3b4a0beed3b421848f93a735f7c
x64win=11c4376e95b89544ddfaf5788afea87ed5b380852ed590a36bb51ca50263cd26
x64mingw=11c4376e95b89544ddfaf5788afea87ed5b380852ed590a36bb51ca50263cd26
x64freebsd=41e52a82ff8082dcd933ddd806ec5ea6d3fd4b7b2242b155961c8821d6360333
x64openbsd=d4d96ee42a27423b3ef8dd1ccb0fdb7a8fd89821436915aeed397519c8a82ee3
x64netbsd=a273aa17c705ef627f32e61aba639fc22c2b1119880003c3e6e0999e66ed3e4e
x64musl=a273aa17c705ef627f32e61aba639fc22c2b1119880003c3e6e0999e66ed3e4e
x64glibc=a273aa17c705ef627f32e61aba639fc22c2b1119880003c3e6e0999e66ed3e4e
x64linux=a273aa17c705ef627f32e61aba639fc22c2b1119880003c3e6e0999e66ed3e4e
x64elf=a273aa17c705ef627f32e61aba639fc22c2b1119880003c3e6e0999e66ed3e4e
x64v1mac=80acd34f0a04bcc8ed2aa7aa4a3e78de612cf3b4a0beed3b421848f93a735f7c
x64v1win=11c4376e95b89544ddfaf5788afea87ed5b380852ed590a36bb51ca50263cd26
x64v1mingw=11c4376e95b89544ddfaf5788afea87ed5b380852ed590a36bb51ca50263cd26
x64v1freebsd=41e52a82ff8082dcd933ddd806ec5ea6d3fd4b7b2242b155961c8821d6360333
x64v1openbsd=d4d96ee42a27423b3ef8dd1ccb0fdb7a8fd89821436915aeed397519c8a82ee3
x64v1netbsd=a273aa17c705ef627f32e61aba639fc22c2b1119880003c3e6e0999e66ed3e4e
x64v1musl=a273aa17c705ef627f32e61aba639fc22c2b1119880003c3e6e0999e66ed3e4e
x64v1glibc=a273aa17c705ef627f32e61aba639fc22c2b1119880003c3e6e0999e66ed3e4e
x64v1linux=a273aa17c705ef627f32e61aba639fc22c2b1119880003c3e6e0999e66ed3e4e
x64v1elf=a273aa17c705ef627f32e61aba639fc22c2b1119880003c3e6e0999e66ed3e4e
arm64mac=8638c347d220d0e779814b28a1af3aad9b3271f9d758bf15b5b362cac29cdb33
arm64win=aa3bcdca0ba3acc358a7efb194497514f9442997dc0028f0ec81e149c8e69a1f
arm64mingw=aa3bcdca0ba3acc358a7efb194497514f9442997dc0028f0ec81e149c8e69a1f
arm64linux=776eada5d7099eaf17cc6d1b01dfd12cbfe57a775f6c7fb583923ed827c76a96
arm64musl=776eada5d7099eaf17cc6d1b01dfd12cbfe57a775f6c7fb583923ed827c76a96
arm64glibc=776eada5d7099eaf17cc6d1b01dfd12cbfe57a775f6c7fb583923ed827c76a96
arm64v1win=aa3bcdca0ba3acc358a7efb194497514f9442997dc0028f0ec81e149c8e69a1f
arm64v1mingw=aa3bcdca0ba3acc358a7efb194497514f9442997dc0028f0ec81e149c8e69a1f
arm64v1linux=776eada5d7099eaf17cc6d1b01dfd12cbfe57a775f6c7fb583923ed827c76a96
arm64v1musl=776eada5d7099eaf17cc6d1b01dfd12cbfe57a775f6c7fb583923ed827c76a96
arm64v1glibc=776eada5d7099eaf17cc6d1b01dfd12cbfe57a775f6c7fb583923ed827c76a96
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
