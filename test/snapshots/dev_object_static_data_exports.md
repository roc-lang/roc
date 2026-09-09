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
x64mac=ea2e183b7e53fd611c309a9c3c96f35c927d04dfb79d721c8df7b1d68d8b82d4
x64win=2183fce6e6ec210cf3d786b7594ab532cca11da606d4d0ca9f006472e9dc5f38
x64mingw=2183fce6e6ec210cf3d786b7594ab532cca11da606d4d0ca9f006472e9dc5f38
x64freebsd=b040ca01b5593a15f79e27b30f9869f45b02f35c8a6e22ab26600f49a0eba806
x64openbsd=eba0918a40f32af0429831a83b5674ea9a2447bdb0481a1299ecc455f929e1fe
x64netbsd=6b86c22662ddcc2b26aaf873a4c9a6968c5d58564dfad68939fd0e8a999fa007
x64musl=6b86c22662ddcc2b26aaf873a4c9a6968c5d58564dfad68939fd0e8a999fa007
x64glibc=6b86c22662ddcc2b26aaf873a4c9a6968c5d58564dfad68939fd0e8a999fa007
x64linux=6b86c22662ddcc2b26aaf873a4c9a6968c5d58564dfad68939fd0e8a999fa007
x64elf=6b86c22662ddcc2b26aaf873a4c9a6968c5d58564dfad68939fd0e8a999fa007
x64v1mac=ea2e183b7e53fd611c309a9c3c96f35c927d04dfb79d721c8df7b1d68d8b82d4
x64v1win=2183fce6e6ec210cf3d786b7594ab532cca11da606d4d0ca9f006472e9dc5f38
x64v1mingw=2183fce6e6ec210cf3d786b7594ab532cca11da606d4d0ca9f006472e9dc5f38
x64v1freebsd=b040ca01b5593a15f79e27b30f9869f45b02f35c8a6e22ab26600f49a0eba806
x64v1openbsd=eba0918a40f32af0429831a83b5674ea9a2447bdb0481a1299ecc455f929e1fe
x64v1netbsd=6b86c22662ddcc2b26aaf873a4c9a6968c5d58564dfad68939fd0e8a999fa007
x64v1musl=6b86c22662ddcc2b26aaf873a4c9a6968c5d58564dfad68939fd0e8a999fa007
x64v1glibc=6b86c22662ddcc2b26aaf873a4c9a6968c5d58564dfad68939fd0e8a999fa007
x64v1linux=6b86c22662ddcc2b26aaf873a4c9a6968c5d58564dfad68939fd0e8a999fa007
x64v1elf=6b86c22662ddcc2b26aaf873a4c9a6968c5d58564dfad68939fd0e8a999fa007
arm64mac=d3b7aacda4443431f93ac9515836de821b8656fe375f03bc300d72b41a7d61f8
arm64win=466f4d51d72cf0aad50be34336eec15835ca5e7e4506491a5a6fa473b5441ce3
arm64mingw=466f4d51d72cf0aad50be34336eec15835ca5e7e4506491a5a6fa473b5441ce3
arm64linux=35e40320f897658a1208f990cffc5a3285c7467fffa7b5af9c332e99543226ec
arm64musl=35e40320f897658a1208f990cffc5a3285c7467fffa7b5af9c332e99543226ec
arm64glibc=35e40320f897658a1208f990cffc5a3285c7467fffa7b5af9c332e99543226ec
arm64v1win=466f4d51d72cf0aad50be34336eec15835ca5e7e4506491a5a6fa473b5441ce3
arm64v1mingw=466f4d51d72cf0aad50be34336eec15835ca5e7e4506491a5a6fa473b5441ce3
arm64v1linux=35e40320f897658a1208f990cffc5a3285c7467fffa7b5af9c332e99543226ec
arm64v1musl=35e40320f897658a1208f990cffc5a3285c7467fffa7b5af9c332e99543226ec
arm64v1glibc=35e40320f897658a1208f990cffc5a3285c7467fffa7b5af9c332e99543226ec
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
