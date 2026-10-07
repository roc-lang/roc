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
x64mac=befa8addda8e771b3dc265032f5c676e23691db0fe6fbc4ec7b49b5a083a08c8
x64win=feebc85d0c318d762c74e91201ddea24b012dc080a143f77780aba89855fd265
x64mingw=feebc85d0c318d762c74e91201ddea24b012dc080a143f77780aba89855fd265
x64freebsd=599278adde40a4c8904635c28844a4348d1be398b8de87f452c43365394377b3
x64openbsd=a149be405b382f70f514a41a51eb447543b4bfe4224cc3a41ba1c6436b96c1d6
x64netbsd=936d5bb1d1f2d8121921395441549eb93877e5028361835da8c2a1db360a772a
x64musl=936d5bb1d1f2d8121921395441549eb93877e5028361835da8c2a1db360a772a
x64glibc=936d5bb1d1f2d8121921395441549eb93877e5028361835da8c2a1db360a772a
x64linux=936d5bb1d1f2d8121921395441549eb93877e5028361835da8c2a1db360a772a
x64elf=936d5bb1d1f2d8121921395441549eb93877e5028361835da8c2a1db360a772a
x64v1mac=befa8addda8e771b3dc265032f5c676e23691db0fe6fbc4ec7b49b5a083a08c8
x64v1win=feebc85d0c318d762c74e91201ddea24b012dc080a143f77780aba89855fd265
x64v1mingw=feebc85d0c318d762c74e91201ddea24b012dc080a143f77780aba89855fd265
x64v1freebsd=599278adde40a4c8904635c28844a4348d1be398b8de87f452c43365394377b3
x64v1openbsd=a149be405b382f70f514a41a51eb447543b4bfe4224cc3a41ba1c6436b96c1d6
x64v1netbsd=936d5bb1d1f2d8121921395441549eb93877e5028361835da8c2a1db360a772a
x64v1musl=936d5bb1d1f2d8121921395441549eb93877e5028361835da8c2a1db360a772a
x64v1glibc=936d5bb1d1f2d8121921395441549eb93877e5028361835da8c2a1db360a772a
x64v1linux=936d5bb1d1f2d8121921395441549eb93877e5028361835da8c2a1db360a772a
x64v1elf=936d5bb1d1f2d8121921395441549eb93877e5028361835da8c2a1db360a772a
arm64mac=ca4169c71c896f67301ab1a53a0d4a1c4943ade2f77f43791b5e640e4c3b06aa
arm64win=afb60a02e79efdf04626bf6bd0c5216d218c0ae06ff4005ec70a7c1054f374f3
arm64mingw=afb60a02e79efdf04626bf6bd0c5216d218c0ae06ff4005ec70a7c1054f374f3
arm64linux=9eee04ee33345513c333336ecc81bd39c86df3694a66b7522c68c529760def3e
arm64musl=9eee04ee33345513c333336ecc81bd39c86df3694a66b7522c68c529760def3e
arm64glibc=9eee04ee33345513c333336ecc81bd39c86df3694a66b7522c68c529760def3e
arm64v1win=afb60a02e79efdf04626bf6bd0c5216d218c0ae06ff4005ec70a7c1054f374f3
arm64v1mingw=afb60a02e79efdf04626bf6bd0c5216d218c0ae06ff4005ec70a7c1054f374f3
arm64v1linux=9eee04ee33345513c333336ecc81bd39c86df3694a66b7522c68c529760def3e
arm64v1musl=9eee04ee33345513c333336ecc81bd39c86df3694a66b7522c68c529760def3e
arm64v1glibc=9eee04ee33345513c333336ecc81bd39c86df3694a66b7522c68c529760def3e
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
