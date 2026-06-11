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
        answer_for_host: "answer",
        table_for_host: "table",
        names_for_host: "names",
        tree_for_host: "tree",
    }
    targets: {
        inputs: "targets/",
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
x64mac=79b181c9ca365117fa88ed8df5b4f8decda29f58b6854b3c5d025ada1555ecd0
x64win=b94ee67251b8b99f8fea5ad49511bc600694298d186e8b73ab67167d8cc28a49
x64freebsd=5a8c1a31b036f4bd7a8c44cc2df1b604c967047ba1c9e617c991b1bb06ad66df
x64openbsd=5a8c1a31b036f4bd7a8c44cc2df1b604c967047ba1c9e617c991b1bb06ad66df
x64netbsd=5a8c1a31b036f4bd7a8c44cc2df1b604c967047ba1c9e617c991b1bb06ad66df
x64musl=5a8c1a31b036f4bd7a8c44cc2df1b604c967047ba1c9e617c991b1bb06ad66df
x64glibc=5a8c1a31b036f4bd7a8c44cc2df1b604c967047ba1c9e617c991b1bb06ad66df
x64linux=5a8c1a31b036f4bd7a8c44cc2df1b604c967047ba1c9e617c991b1bb06ad66df
x64elf=5a8c1a31b036f4bd7a8c44cc2df1b604c967047ba1c9e617c991b1bb06ad66df
arm64mac=0f0aa36c4a2cff075cba37db2a278e86d08388a64a903ce2e9ccf4d4962b955a
arm64win=349b3649e6345b9a39814f7fa28d3b1854bfa99430c99dab5fbf8c8a5f1654e6
arm64linux=116495fb2f4899c501b24644a238126d699e340e7f020fd6908c7b9dd4c45c99
arm64musl=116495fb2f4899c501b24644a238126d699e340e7f020fd6908c7b9dd4c45c99
arm64glibc=116495fb2f4899c501b24644a238126d699e340e7f020fd6908c7b9dd4c45c99
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
