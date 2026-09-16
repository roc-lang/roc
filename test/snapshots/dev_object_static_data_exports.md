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
x64mac=5dad2c5ddc8ac1c6a645c957fbd2734cc409d124ba234f07e56a55365a13ec01
x64win=76c467ebed34a5741eea1651d75fcb2246632647b24d867488fd67d485005f11
x64mingw=76c467ebed34a5741eea1651d75fcb2246632647b24d867488fd67d485005f11
x64freebsd=55fca1efedb795d52837e3f3aa53290da55c3a4233b674d75e6262edb2c88c57
x64openbsd=14952ddc587fbfb630f527f3e9f04c239f5c745e86c97cf2069fde1b6ae05572
x64netbsd=3952df06f2638e888ace545bb63d839a7d00f1d422df6e78fac9c3f135d42f62
x64musl=3952df06f2638e888ace545bb63d839a7d00f1d422df6e78fac9c3f135d42f62
x64glibc=3952df06f2638e888ace545bb63d839a7d00f1d422df6e78fac9c3f135d42f62
x64linux=3952df06f2638e888ace545bb63d839a7d00f1d422df6e78fac9c3f135d42f62
x64elf=3952df06f2638e888ace545bb63d839a7d00f1d422df6e78fac9c3f135d42f62
x64v1mac=5dad2c5ddc8ac1c6a645c957fbd2734cc409d124ba234f07e56a55365a13ec01
x64v1win=76c467ebed34a5741eea1651d75fcb2246632647b24d867488fd67d485005f11
x64v1mingw=76c467ebed34a5741eea1651d75fcb2246632647b24d867488fd67d485005f11
x64v1freebsd=55fca1efedb795d52837e3f3aa53290da55c3a4233b674d75e6262edb2c88c57
x64v1openbsd=14952ddc587fbfb630f527f3e9f04c239f5c745e86c97cf2069fde1b6ae05572
x64v1netbsd=3952df06f2638e888ace545bb63d839a7d00f1d422df6e78fac9c3f135d42f62
x64v1musl=3952df06f2638e888ace545bb63d839a7d00f1d422df6e78fac9c3f135d42f62
x64v1glibc=3952df06f2638e888ace545bb63d839a7d00f1d422df6e78fac9c3f135d42f62
x64v1linux=3952df06f2638e888ace545bb63d839a7d00f1d422df6e78fac9c3f135d42f62
x64v1elf=3952df06f2638e888ace545bb63d839a7d00f1d422df6e78fac9c3f135d42f62
arm64mac=f46685bbee38119972ec1813bbad718926b306cc1c48f054e07a238c3e23305c
arm64win=bf2b202644b4514dbe03d5443981f71c8f29c1f4c835841e9c578fa43d7306ca
arm64mingw=bf2b202644b4514dbe03d5443981f71c8f29c1f4c835841e9c578fa43d7306ca
arm64linux=ceee62f24305e61393339532db65825beaa8f747e98665de416b6a9da43b5fa1
arm64musl=ceee62f24305e61393339532db65825beaa8f747e98665de416b6a9da43b5fa1
arm64glibc=ceee62f24305e61393339532db65825beaa8f747e98665de416b6a9da43b5fa1
arm64v1win=bf2b202644b4514dbe03d5443981f71c8f29c1f4c835841e9c578fa43d7306ca
arm64v1mingw=bf2b202644b4514dbe03d5443981f71c8f29c1f4c835841e9c578fa43d7306ca
arm64v1linux=ceee62f24305e61393339532db65825beaa8f747e98665de416b6a9da43b5fa1
arm64v1musl=ceee62f24305e61393339532db65825beaa8f747e98665de416b6a9da43b5fa1
arm64v1glibc=ceee62f24305e61393339532db65825beaa8f747e98665de416b6a9da43b5fa1
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
