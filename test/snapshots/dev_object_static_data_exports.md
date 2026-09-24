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
x64mac=248ee160e8c57931126b09ab62ed1fb80c1ee6bcf3f1e83e4ca92d3353e889f1
x64win=c6b96cb78e1aa90c84ea1d57974732239ea40fd84d77b09e5e2b719ca7c8e1a8
x64mingw=c6b96cb78e1aa90c84ea1d57974732239ea40fd84d77b09e5e2b719ca7c8e1a8
x64freebsd=69f68e98c3204bf27f439d8b095460b091a55028d6f0959830de27757dc64e4f
x64openbsd=5e361f52f03550a344a56df1db5821405aa7703e05dd31c5473456a710d57cd2
x64netbsd=e3479c7c491cb909cc14ca8c7af614c3fad6e893d12125770e0bf818d26455da
x64musl=e3479c7c491cb909cc14ca8c7af614c3fad6e893d12125770e0bf818d26455da
x64glibc=e3479c7c491cb909cc14ca8c7af614c3fad6e893d12125770e0bf818d26455da
x64linux=e3479c7c491cb909cc14ca8c7af614c3fad6e893d12125770e0bf818d26455da
x64elf=e3479c7c491cb909cc14ca8c7af614c3fad6e893d12125770e0bf818d26455da
x64v1mac=248ee160e8c57931126b09ab62ed1fb80c1ee6bcf3f1e83e4ca92d3353e889f1
x64v1win=c6b96cb78e1aa90c84ea1d57974732239ea40fd84d77b09e5e2b719ca7c8e1a8
x64v1mingw=c6b96cb78e1aa90c84ea1d57974732239ea40fd84d77b09e5e2b719ca7c8e1a8
x64v1freebsd=69f68e98c3204bf27f439d8b095460b091a55028d6f0959830de27757dc64e4f
x64v1openbsd=5e361f52f03550a344a56df1db5821405aa7703e05dd31c5473456a710d57cd2
x64v1netbsd=e3479c7c491cb909cc14ca8c7af614c3fad6e893d12125770e0bf818d26455da
x64v1musl=e3479c7c491cb909cc14ca8c7af614c3fad6e893d12125770e0bf818d26455da
x64v1glibc=e3479c7c491cb909cc14ca8c7af614c3fad6e893d12125770e0bf818d26455da
x64v1linux=e3479c7c491cb909cc14ca8c7af614c3fad6e893d12125770e0bf818d26455da
x64v1elf=e3479c7c491cb909cc14ca8c7af614c3fad6e893d12125770e0bf818d26455da
arm64mac=d4d4123db68e001c0d440ffe237939ed07b3e7dccc29918830fa3f450ef01fb7
arm64win=62326dda273ce20ac9a40fcaa00d1563e5eb4fe121996d62886e413035fdea59
arm64mingw=62326dda273ce20ac9a40fcaa00d1563e5eb4fe121996d62886e413035fdea59
arm64linux=1da503a32de44662db23df7091626ec40b4d7f6cd502a97613b945e89caa6646
arm64musl=1da503a32de44662db23df7091626ec40b4d7f6cd502a97613b945e89caa6646
arm64glibc=1da503a32de44662db23df7091626ec40b4d7f6cd502a97613b945e89caa6646
arm64v1win=62326dda273ce20ac9a40fcaa00d1563e5eb4fe121996d62886e413035fdea59
arm64v1mingw=62326dda273ce20ac9a40fcaa00d1563e5eb4fe121996d62886e413035fdea59
arm64v1linux=1da503a32de44662db23df7091626ec40b4d7f6cd502a97613b945e89caa6646
arm64v1musl=1da503a32de44662db23df7091626ec40b4d7f6cd502a97613b945e89caa6646
arm64v1glibc=1da503a32de44662db23df7091626ec40b4d7f6cd502a97613b945e89caa6646
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
