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
x64mac=5841ee261180bf754b0b08cb90bfd216e063ce89f85d3bf35ef817a9d30bace1
x64win=db4fd2034faf992b85c0106377401ad98f50b387b6bc01650311217baf8b32ac
x64mingw=db4fd2034faf992b85c0106377401ad98f50b387b6bc01650311217baf8b32ac
x64freebsd=a534e59c3e26501c534bfa30679cbc3e3429501ae9607000d425b740527d7470
x64openbsd=d16ad0feba327973ac4ccf9f519ea42e9b3e763480a7193f0d54c30b9be7e29d
x64netbsd=6911753274beca6d5dbdae6cec774acde0bfdd327e108978ca290d86f3f723f1
x64musl=6911753274beca6d5dbdae6cec774acde0bfdd327e108978ca290d86f3f723f1
x64glibc=6911753274beca6d5dbdae6cec774acde0bfdd327e108978ca290d86f3f723f1
x64linux=6911753274beca6d5dbdae6cec774acde0bfdd327e108978ca290d86f3f723f1
x64elf=6911753274beca6d5dbdae6cec774acde0bfdd327e108978ca290d86f3f723f1
x64v1mac=5841ee261180bf754b0b08cb90bfd216e063ce89f85d3bf35ef817a9d30bace1
x64v1win=db4fd2034faf992b85c0106377401ad98f50b387b6bc01650311217baf8b32ac
x64v1mingw=db4fd2034faf992b85c0106377401ad98f50b387b6bc01650311217baf8b32ac
x64v1freebsd=a534e59c3e26501c534bfa30679cbc3e3429501ae9607000d425b740527d7470
x64v1openbsd=d16ad0feba327973ac4ccf9f519ea42e9b3e763480a7193f0d54c30b9be7e29d
x64v1netbsd=6911753274beca6d5dbdae6cec774acde0bfdd327e108978ca290d86f3f723f1
x64v1musl=6911753274beca6d5dbdae6cec774acde0bfdd327e108978ca290d86f3f723f1
x64v1glibc=6911753274beca6d5dbdae6cec774acde0bfdd327e108978ca290d86f3f723f1
x64v1linux=6911753274beca6d5dbdae6cec774acde0bfdd327e108978ca290d86f3f723f1
x64v1elf=6911753274beca6d5dbdae6cec774acde0bfdd327e108978ca290d86f3f723f1
arm64mac=3c309a958bd1ccd17a460fd9e2259ee56b9397507211843f842a350a6595c767
arm64win=36f632ea1048459b570c7c727ce50951f70b1a4e3b12549424526e5142b39da9
arm64mingw=36f632ea1048459b570c7c727ce50951f70b1a4e3b12549424526e5142b39da9
arm64linux=724590256d0e11dc0d0cf5b4b7a83968a237bc14f24696b4dd4e963ff433ce26
arm64musl=724590256d0e11dc0d0cf5b4b7a83968a237bc14f24696b4dd4e963ff433ce26
arm64glibc=724590256d0e11dc0d0cf5b4b7a83968a237bc14f24696b4dd4e963ff433ce26
arm64v1win=36f632ea1048459b570c7c727ce50951f70b1a4e3b12549424526e5142b39da9
arm64v1mingw=36f632ea1048459b570c7c727ce50951f70b1a4e3b12549424526e5142b39da9
arm64v1linux=724590256d0e11dc0d0cf5b4b7a83968a237bc14f24696b4dd4e963ff433ce26
arm64v1musl=724590256d0e11dc0d0cf5b4b7a83968a237bc14f24696b4dd4e963ff433ce26
arm64v1glibc=724590256d0e11dc0d0cf5b4b7a83968a237bc14f24696b4dd4e963ff433ce26
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
