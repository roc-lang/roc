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
x64mac=1fb43df9a7e6111ca58da18b4a8dca2074199d165dcee2d10cfc12bfe4cc4394
x64win=7520d6bce2d2480bd3a521960cc558a67c0812b676234b42bd25f30fd5f0336e
x64freebsd=45ddb86fb81ed64230b6ce4cde65ff3328cbc47672ad05ed6b489b59a30b79fc
x64openbsd=45ddb86fb81ed64230b6ce4cde65ff3328cbc47672ad05ed6b489b59a30b79fc
x64netbsd=45ddb86fb81ed64230b6ce4cde65ff3328cbc47672ad05ed6b489b59a30b79fc
x64musl=45ddb86fb81ed64230b6ce4cde65ff3328cbc47672ad05ed6b489b59a30b79fc
x64glibc=45ddb86fb81ed64230b6ce4cde65ff3328cbc47672ad05ed6b489b59a30b79fc
x64linux=45ddb86fb81ed64230b6ce4cde65ff3328cbc47672ad05ed6b489b59a30b79fc
x64elf=45ddb86fb81ed64230b6ce4cde65ff3328cbc47672ad05ed6b489b59a30b79fc
arm64mac=d2c0684d100ce06b1e393529d53340be3f4249518edfc89856dd566fc6a12713
arm64win=68fc0695aa9582dac5a39252d6aa7f18a5e508f7ba692874711f9807db53023e
arm64linux=c7dc811fa15d045580daa20f6b68dcfb48719d400af4d9ee38a12cd409eb0f7f
arm64musl=c7dc811fa15d045580daa20f6b68dcfb48719d400af4d9ee38a12cd409eb0f7f
arm64glibc=c7dc811fa15d045580daa20f6b68dcfb48719d400af4d9ee38a12cd409eb0f7f
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
