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
x64mac=2e293d7ee2b75e8eed6156bfbbd0d4a97ebae4a2f78c1a0b5cb7142e62e5aa92
x64win=72f086619ef46b17eb61b5bb4c96d1eba6095e208a146a6129202dcbebf7ff15
x64mingw=72f086619ef46b17eb61b5bb4c96d1eba6095e208a146a6129202dcbebf7ff15
x64freebsd=901ec2cc542c3b90cd00756633da3450f5202ded45bafb7f6521eb35d798fa74
x64openbsd=57482425f0117c38378018e4d2f077384b908c5228ba0602781e630a7857d87b
x64netbsd=86b568764e1b1a321cbe4964384cf89dd63caa56fa109709e84b6efcb85db585
x64musl=86b568764e1b1a321cbe4964384cf89dd63caa56fa109709e84b6efcb85db585
x64glibc=86b568764e1b1a321cbe4964384cf89dd63caa56fa109709e84b6efcb85db585
x64linux=86b568764e1b1a321cbe4964384cf89dd63caa56fa109709e84b6efcb85db585
x64elf=86b568764e1b1a321cbe4964384cf89dd63caa56fa109709e84b6efcb85db585
x64v1mac=2e293d7ee2b75e8eed6156bfbbd0d4a97ebae4a2f78c1a0b5cb7142e62e5aa92
x64v1win=72f086619ef46b17eb61b5bb4c96d1eba6095e208a146a6129202dcbebf7ff15
x64v1mingw=72f086619ef46b17eb61b5bb4c96d1eba6095e208a146a6129202dcbebf7ff15
x64v1freebsd=901ec2cc542c3b90cd00756633da3450f5202ded45bafb7f6521eb35d798fa74
x64v1openbsd=57482425f0117c38378018e4d2f077384b908c5228ba0602781e630a7857d87b
x64v1netbsd=86b568764e1b1a321cbe4964384cf89dd63caa56fa109709e84b6efcb85db585
x64v1musl=86b568764e1b1a321cbe4964384cf89dd63caa56fa109709e84b6efcb85db585
x64v1glibc=86b568764e1b1a321cbe4964384cf89dd63caa56fa109709e84b6efcb85db585
x64v1linux=86b568764e1b1a321cbe4964384cf89dd63caa56fa109709e84b6efcb85db585
x64v1elf=86b568764e1b1a321cbe4964384cf89dd63caa56fa109709e84b6efcb85db585
arm64mac=1ba0971a9c539ca124b66ccd8f49a47d01ea43d71c1f6e2e122c6f28a624eed9
arm64win=ac9c6fbff4758404c8133ad570deba5ead676488d739584d3e4214044975722d
arm64mingw=ac9c6fbff4758404c8133ad570deba5ead676488d739584d3e4214044975722d
arm64linux=2c0b48884374a6e4089033904aa18970551a42431b77c6e14158b0ef8dd38b8d
arm64musl=2c0b48884374a6e4089033904aa18970551a42431b77c6e14158b0ef8dd38b8d
arm64glibc=2c0b48884374a6e4089033904aa18970551a42431b77c6e14158b0ef8dd38b8d
arm64v1win=ac9c6fbff4758404c8133ad570deba5ead676488d739584d3e4214044975722d
arm64v1mingw=ac9c6fbff4758404c8133ad570deba5ead676488d739584d3e4214044975722d
arm64v1linux=2c0b48884374a6e4089033904aa18970551a42431b77c6e14158b0ef8dd38b8d
arm64v1musl=2c0b48884374a6e4089033904aa18970551a42431b77c6e14158b0ef8dd38b8d
arm64v1glibc=2c0b48884374a6e4089033904aa18970551a42431b77c6e14158b0ef8dd38b8d
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
