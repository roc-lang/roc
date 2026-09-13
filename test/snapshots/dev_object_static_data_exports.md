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
x64mac=f9a855ca730c8bd103e7469b21df46f7630858f6ea8bb847443edcb5458496d7
x64win=4384b9b41f2f338c78bf1a87f2a1c5dbced6d9a6c897f0df65fd0f12f7bc33cb
x64mingw=4384b9b41f2f338c78bf1a87f2a1c5dbced6d9a6c897f0df65fd0f12f7bc33cb
x64freebsd=cea3e0a21bbd0d7b8a9893bd3655f8d33ebde3a057a06e58d2948a3aa9b7f20a
x64openbsd=308de7eb61f346ca11cfc0063f6fb262a2c147a83720d60fd254d42401b3c397
x64netbsd=4af07135af93cd24b977ef7ce3104f83623b0dc7c7306effaa85d473a3b16e7c
x64musl=4af07135af93cd24b977ef7ce3104f83623b0dc7c7306effaa85d473a3b16e7c
x64glibc=4af07135af93cd24b977ef7ce3104f83623b0dc7c7306effaa85d473a3b16e7c
x64linux=4af07135af93cd24b977ef7ce3104f83623b0dc7c7306effaa85d473a3b16e7c
x64elf=4af07135af93cd24b977ef7ce3104f83623b0dc7c7306effaa85d473a3b16e7c
x64v1mac=f9a855ca730c8bd103e7469b21df46f7630858f6ea8bb847443edcb5458496d7
x64v1win=4384b9b41f2f338c78bf1a87f2a1c5dbced6d9a6c897f0df65fd0f12f7bc33cb
x64v1mingw=4384b9b41f2f338c78bf1a87f2a1c5dbced6d9a6c897f0df65fd0f12f7bc33cb
x64v1freebsd=cea3e0a21bbd0d7b8a9893bd3655f8d33ebde3a057a06e58d2948a3aa9b7f20a
x64v1openbsd=308de7eb61f346ca11cfc0063f6fb262a2c147a83720d60fd254d42401b3c397
x64v1netbsd=4af07135af93cd24b977ef7ce3104f83623b0dc7c7306effaa85d473a3b16e7c
x64v1musl=4af07135af93cd24b977ef7ce3104f83623b0dc7c7306effaa85d473a3b16e7c
x64v1glibc=4af07135af93cd24b977ef7ce3104f83623b0dc7c7306effaa85d473a3b16e7c
x64v1linux=4af07135af93cd24b977ef7ce3104f83623b0dc7c7306effaa85d473a3b16e7c
x64v1elf=4af07135af93cd24b977ef7ce3104f83623b0dc7c7306effaa85d473a3b16e7c
arm64mac=fbd734ab570e599f36f6dc57d286235a0547bfc08a4f84653a46681b6585a4b2
arm64win=7239bd109d4c15a084712080b772c38782aa5cdd90a0af9b5cc0cbf9148d0349
arm64mingw=7239bd109d4c15a084712080b772c38782aa5cdd90a0af9b5cc0cbf9148d0349
arm64linux=9d17786aca57ccea8b17d391bafab7f5b892436b14b71a353bc7772babe219eb
arm64musl=9d17786aca57ccea8b17d391bafab7f5b892436b14b71a353bc7772babe219eb
arm64glibc=9d17786aca57ccea8b17d391bafab7f5b892436b14b71a353bc7772babe219eb
arm64v1win=7239bd109d4c15a084712080b772c38782aa5cdd90a0af9b5cc0cbf9148d0349
arm64v1mingw=7239bd109d4c15a084712080b772c38782aa5cdd90a0af9b5cc0cbf9148d0349
arm64v1linux=9d17786aca57ccea8b17d391bafab7f5b892436b14b71a353bc7772babe219eb
arm64v1musl=9d17786aca57ccea8b17d391bafab7f5b892436b14b71a353bc7772babe219eb
arm64v1glibc=9d17786aca57ccea8b17d391bafab7f5b892436b14b71a353bc7772babe219eb
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
