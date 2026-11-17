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
x64mac=0751fc1d283fcedcf32a1db1b02b8a886dad30d4e8dbf57ae4e81416cdeb6c99
x64win=9ee773d822961129dffef218a5b93cf6256bbd2e0c737f644b7d359af4370c67
x64mingw=9ee773d822961129dffef218a5b93cf6256bbd2e0c737f644b7d359af4370c67
x64freebsd=ef7dcd47ad02d2549df8f61b9e028cd198b5d6fad46c71df9e6da59a33f52429
x64openbsd=b2e19c11f7009dcd04af74c36d175d0035546ae2abb663e93648c1c679f802ab
x64netbsd=2807ecac9dfd4b424dade906e7c3263a439c2925e5c2642332938edeec262cc0
x64musl=2807ecac9dfd4b424dade906e7c3263a439c2925e5c2642332938edeec262cc0
x64glibc=2807ecac9dfd4b424dade906e7c3263a439c2925e5c2642332938edeec262cc0
x64linux=2807ecac9dfd4b424dade906e7c3263a439c2925e5c2642332938edeec262cc0
x64elf=2807ecac9dfd4b424dade906e7c3263a439c2925e5c2642332938edeec262cc0
x64v1mac=0751fc1d283fcedcf32a1db1b02b8a886dad30d4e8dbf57ae4e81416cdeb6c99
x64v1win=9ee773d822961129dffef218a5b93cf6256bbd2e0c737f644b7d359af4370c67
x64v1mingw=9ee773d822961129dffef218a5b93cf6256bbd2e0c737f644b7d359af4370c67
x64v1freebsd=ef7dcd47ad02d2549df8f61b9e028cd198b5d6fad46c71df9e6da59a33f52429
x64v1openbsd=b2e19c11f7009dcd04af74c36d175d0035546ae2abb663e93648c1c679f802ab
x64v1netbsd=2807ecac9dfd4b424dade906e7c3263a439c2925e5c2642332938edeec262cc0
x64v1musl=2807ecac9dfd4b424dade906e7c3263a439c2925e5c2642332938edeec262cc0
x64v1glibc=2807ecac9dfd4b424dade906e7c3263a439c2925e5c2642332938edeec262cc0
x64v1linux=2807ecac9dfd4b424dade906e7c3263a439c2925e5c2642332938edeec262cc0
x64v1elf=2807ecac9dfd4b424dade906e7c3263a439c2925e5c2642332938edeec262cc0
arm64mac=f803c41674eaf8af06d76edeb5ad65f916d6fe25d8a4c0aab9430cdfebef7acd
arm64win=936b7b5b44789b6708a960a268fe1ffc331dee1f762420f9a2e6c3700d5fc74f
arm64mingw=936b7b5b44789b6708a960a268fe1ffc331dee1f762420f9a2e6c3700d5fc74f
arm64linux=4f273abb975a66ea645241a218154c528c26dc5d821eb4cc8537ac8ca04546a8
arm64musl=4f273abb975a66ea645241a218154c528c26dc5d821eb4cc8537ac8ca04546a8
arm64glibc=4f273abb975a66ea645241a218154c528c26dc5d821eb4cc8537ac8ca04546a8
arm64v1win=936b7b5b44789b6708a960a268fe1ffc331dee1f762420f9a2e6c3700d5fc74f
arm64v1mingw=936b7b5b44789b6708a960a268fe1ffc331dee1f762420f9a2e6c3700d5fc74f
arm64v1linux=4f273abb975a66ea645241a218154c528c26dc5d821eb4cc8537ac8ca04546a8
arm64v1musl=4f273abb975a66ea645241a218154c528c26dc5d821eb4cc8537ac8ca04546a8
arm64v1glibc=4f273abb975a66ea645241a218154c528c26dc5d821eb4cc8537ac8ca04546a8
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
