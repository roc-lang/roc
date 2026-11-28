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
x64mac=534255198190fd86664c3e618b14cb709ce2b4f95b331e0999f4ec7258367db4
x64win=2bd7bb75ec98af2a3e3ba359b720872be7987662db36498474514b58fe4e49de
x64mingw=2bd7bb75ec98af2a3e3ba359b720872be7987662db36498474514b58fe4e49de
x64freebsd=81c1813f1df76df8cef60901369621e0d79eb79878e2006522e12853a4bb2411
x64openbsd=3774118e32f2682743e61c26bd7dd455f35ac68e779df08e77523cac11c05381
x64netbsd=33d0b47b868f838a8bde1b54e2d7a36e9a12edd848189096b6091b3deefcb7fe
x64musl=33d0b47b868f838a8bde1b54e2d7a36e9a12edd848189096b6091b3deefcb7fe
x64glibc=33d0b47b868f838a8bde1b54e2d7a36e9a12edd848189096b6091b3deefcb7fe
x64linux=33d0b47b868f838a8bde1b54e2d7a36e9a12edd848189096b6091b3deefcb7fe
x64elf=33d0b47b868f838a8bde1b54e2d7a36e9a12edd848189096b6091b3deefcb7fe
x64v1mac=534255198190fd86664c3e618b14cb709ce2b4f95b331e0999f4ec7258367db4
x64v1win=2bd7bb75ec98af2a3e3ba359b720872be7987662db36498474514b58fe4e49de
x64v1mingw=2bd7bb75ec98af2a3e3ba359b720872be7987662db36498474514b58fe4e49de
x64v1freebsd=81c1813f1df76df8cef60901369621e0d79eb79878e2006522e12853a4bb2411
x64v1openbsd=3774118e32f2682743e61c26bd7dd455f35ac68e779df08e77523cac11c05381
x64v1netbsd=33d0b47b868f838a8bde1b54e2d7a36e9a12edd848189096b6091b3deefcb7fe
x64v1musl=33d0b47b868f838a8bde1b54e2d7a36e9a12edd848189096b6091b3deefcb7fe
x64v1glibc=33d0b47b868f838a8bde1b54e2d7a36e9a12edd848189096b6091b3deefcb7fe
x64v1linux=33d0b47b868f838a8bde1b54e2d7a36e9a12edd848189096b6091b3deefcb7fe
x64v1elf=33d0b47b868f838a8bde1b54e2d7a36e9a12edd848189096b6091b3deefcb7fe
arm64mac=8b3aad1e3a0fd76c31f96ff47a85ea18790d2e299a005d4bda2dd57159a78bd4
arm64win=90ea43b33f8150e1812f63b59577db60b0c239531e683a7b24366652f2c494e5
arm64mingw=90ea43b33f8150e1812f63b59577db60b0c239531e683a7b24366652f2c494e5
arm64linux=f5d594a19f8ae169ac02804691bd8dc9bf20934ae0056ac53702db55df86fa94
arm64musl=f5d594a19f8ae169ac02804691bd8dc9bf20934ae0056ac53702db55df86fa94
arm64glibc=f5d594a19f8ae169ac02804691bd8dc9bf20934ae0056ac53702db55df86fa94
arm64v1win=90ea43b33f8150e1812f63b59577db60b0c239531e683a7b24366652f2c494e5
arm64v1mingw=90ea43b33f8150e1812f63b59577db60b0c239531e683a7b24366652f2c494e5
arm64v1linux=f5d594a19f8ae169ac02804691bd8dc9bf20934ae0056ac53702db55df86fa94
arm64v1musl=f5d594a19f8ae169ac02804691bd8dc9bf20934ae0056ac53702db55df86fa94
arm64v1glibc=f5d594a19f8ae169ac02804691bd8dc9bf20934ae0056ac53702db55df86fa94
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
