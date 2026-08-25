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
x64mac=32577d84e1f725cdfe30a42672e95f2d42bd357ea578afe58dee64cc51f0ce0c
x64win=13aecde39ed400d3902b16fa34dc4ad804cf4fe8db8c26c8d70a03c054f79bd1
x64mingw=13aecde39ed400d3902b16fa34dc4ad804cf4fe8db8c26c8d70a03c054f79bd1
x64freebsd=cca66385360934616dca9afe62c7886d5fceba95e1785024fd4077703f7ca89c
x64openbsd=e8a49d3f0b745bd9243c60839e49b279bf61d836c91dbeeb0cf09bd9a341b06e
x64netbsd=e804a7973cb69a351a2e5e88b988b682e117926769ce0c37a8bf3f3f03138a52
x64musl=e804a7973cb69a351a2e5e88b988b682e117926769ce0c37a8bf3f3f03138a52
x64glibc=e804a7973cb69a351a2e5e88b988b682e117926769ce0c37a8bf3f3f03138a52
x64linux=e804a7973cb69a351a2e5e88b988b682e117926769ce0c37a8bf3f3f03138a52
x64elf=e804a7973cb69a351a2e5e88b988b682e117926769ce0c37a8bf3f3f03138a52
x64v1mac=32577d84e1f725cdfe30a42672e95f2d42bd357ea578afe58dee64cc51f0ce0c
x64v1win=13aecde39ed400d3902b16fa34dc4ad804cf4fe8db8c26c8d70a03c054f79bd1
x64v1mingw=13aecde39ed400d3902b16fa34dc4ad804cf4fe8db8c26c8d70a03c054f79bd1
x64v1freebsd=cca66385360934616dca9afe62c7886d5fceba95e1785024fd4077703f7ca89c
x64v1openbsd=e8a49d3f0b745bd9243c60839e49b279bf61d836c91dbeeb0cf09bd9a341b06e
x64v1netbsd=e804a7973cb69a351a2e5e88b988b682e117926769ce0c37a8bf3f3f03138a52
x64v1musl=e804a7973cb69a351a2e5e88b988b682e117926769ce0c37a8bf3f3f03138a52
x64v1glibc=e804a7973cb69a351a2e5e88b988b682e117926769ce0c37a8bf3f3f03138a52
x64v1linux=e804a7973cb69a351a2e5e88b988b682e117926769ce0c37a8bf3f3f03138a52
x64v1elf=e804a7973cb69a351a2e5e88b988b682e117926769ce0c37a8bf3f3f03138a52
arm64mac=63aae80a56db7742f9700a7accb1b935caeadf9c98e0ce908355b3b91ef7d42c
arm64win=40bb9921df83820e8f195db64c8ba1035a12c7f9e1d5befa34b48f04fac35827
arm64mingw=40bb9921df83820e8f195db64c8ba1035a12c7f9e1d5befa34b48f04fac35827
arm64linux=35dc7266dc6f91fb6339335d5ce293b4790427b0da73d1da0fc74d0f6cad791e
arm64musl=35dc7266dc6f91fb6339335d5ce293b4790427b0da73d1da0fc74d0f6cad791e
arm64glibc=35dc7266dc6f91fb6339335d5ce293b4790427b0da73d1da0fc74d0f6cad791e
arm64v1win=40bb9921df83820e8f195db64c8ba1035a12c7f9e1d5befa34b48f04fac35827
arm64v1mingw=40bb9921df83820e8f195db64c8ba1035a12c7f9e1d5befa34b48f04fac35827
arm64v1linux=35dc7266dc6f91fb6339335d5ce293b4790427b0da73d1da0fc74d0f6cad791e
arm64v1musl=35dc7266dc6f91fb6339335d5ce293b4790427b0da73d1da0fc74d0f6cad791e
arm64v1glibc=35dc7266dc6f91fb6339335d5ce293b4790427b0da73d1da0fc74d0f6cad791e
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
