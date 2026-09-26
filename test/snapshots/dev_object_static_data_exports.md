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
x64mac=c404c463a7624d78ffa1b76107ec877d85d4f37726ec4e16e84b5edfee2bf66f
x64win=feebc85d0c318d762c74e91201ddea24b012dc080a143f77780aba89855fd265
x64mingw=feebc85d0c318d762c74e91201ddea24b012dc080a143f77780aba89855fd265
x64freebsd=44b0144001762ef00572e5ad7d01cd9b47477a6dbe310c4d3d2b79cbfccd7821
x64openbsd=2831c98acc38b18bbcc0b16d8d1271564609445e54aa5aa2f62619fcad74aab9
x64netbsd=5c04a6f865cc377a089b75a23ff480b2d47401f312eebaccc1dfc5713723bb3a
x64musl=5c04a6f865cc377a089b75a23ff480b2d47401f312eebaccc1dfc5713723bb3a
x64glibc=5c04a6f865cc377a089b75a23ff480b2d47401f312eebaccc1dfc5713723bb3a
x64linux=5c04a6f865cc377a089b75a23ff480b2d47401f312eebaccc1dfc5713723bb3a
x64elf=5c04a6f865cc377a089b75a23ff480b2d47401f312eebaccc1dfc5713723bb3a
x64v1mac=c404c463a7624d78ffa1b76107ec877d85d4f37726ec4e16e84b5edfee2bf66f
x64v1win=feebc85d0c318d762c74e91201ddea24b012dc080a143f77780aba89855fd265
x64v1mingw=feebc85d0c318d762c74e91201ddea24b012dc080a143f77780aba89855fd265
x64v1freebsd=44b0144001762ef00572e5ad7d01cd9b47477a6dbe310c4d3d2b79cbfccd7821
x64v1openbsd=2831c98acc38b18bbcc0b16d8d1271564609445e54aa5aa2f62619fcad74aab9
x64v1netbsd=5c04a6f865cc377a089b75a23ff480b2d47401f312eebaccc1dfc5713723bb3a
x64v1musl=5c04a6f865cc377a089b75a23ff480b2d47401f312eebaccc1dfc5713723bb3a
x64v1glibc=5c04a6f865cc377a089b75a23ff480b2d47401f312eebaccc1dfc5713723bb3a
x64v1linux=5c04a6f865cc377a089b75a23ff480b2d47401f312eebaccc1dfc5713723bb3a
x64v1elf=5c04a6f865cc377a089b75a23ff480b2d47401f312eebaccc1dfc5713723bb3a
arm64mac=b6b5208f7d1abcb2baa06c12c362be1fbdd3ad3b85b4b8cb33bf46a97bff6a5d
arm64win=afb60a02e79efdf04626bf6bd0c5216d218c0ae06ff4005ec70a7c1054f374f3
arm64mingw=afb60a02e79efdf04626bf6bd0c5216d218c0ae06ff4005ec70a7c1054f374f3
arm64linux=7aaf62fc5e7a328b54e4b7a1b691564df63ac3954ffabcaeacf720d426b099bf
arm64musl=7aaf62fc5e7a328b54e4b7a1b691564df63ac3954ffabcaeacf720d426b099bf
arm64glibc=7aaf62fc5e7a328b54e4b7a1b691564df63ac3954ffabcaeacf720d426b099bf
arm64v1win=afb60a02e79efdf04626bf6bd0c5216d218c0ae06ff4005ec70a7c1054f374f3
arm64v1mingw=afb60a02e79efdf04626bf6bd0c5216d218c0ae06ff4005ec70a7c1054f374f3
arm64v1linux=7aaf62fc5e7a328b54e4b7a1b691564df63ac3954ffabcaeacf720d426b099bf
arm64v1musl=7aaf62fc5e7a328b54e4b7a1b691564df63ac3954ffabcaeacf720d426b099bf
arm64v1glibc=7aaf62fc5e7a328b54e4b7a1b691564df63ac3954ffabcaeacf720d426b099bf
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
