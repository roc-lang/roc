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
x64mac=4bc657f43c6c0a39142785fa7f78378a58b07011396a93a29fdf48283c66afa0
x64win=06c44523b19e355d569e4a899fe8712476967416abf85d6fdda816a2558ade8d
x64freebsd=ec529e75dc96c08fb87b155aaca7706318f24da1cefc073f3c302edf3e6d71d3
x64openbsd=13711ee66b65a177d259868536ec1d5b16e45004cb47d580ad5e2ea8489c07e8
x64netbsd=b5edd7eef26f73e47b52508799873bf9d0e1147fb700c2f109a416823e42fb07
x64musl=b5edd7eef26f73e47b52508799873bf9d0e1147fb700c2f109a416823e42fb07
x64glibc=b5edd7eef26f73e47b52508799873bf9d0e1147fb700c2f109a416823e42fb07
x64linux=b5edd7eef26f73e47b52508799873bf9d0e1147fb700c2f109a416823e42fb07
x64elf=b5edd7eef26f73e47b52508799873bf9d0e1147fb700c2f109a416823e42fb07
arm64mac=c6d9b5e603a948a6a59082ad8fab5a8f796283698ab565801be45107cd1fd199
arm64win=3cbb4aaefebfbceed61d1edd84aa694aed8d265f6f776e4ec68bb5134e0f32df
arm64linux=2a9418881579fc80b40013cf9cf209216f177cd7f0635bf367d271d04cc7fcd7
arm64musl=2a9418881579fc80b40013cf9cf209216f177cd7f0635bf367d271d04cc7fcd7
arm64glibc=2a9418881579fc80b40013cf9cf209216f177cd7f0635bf367d271d04cc7fcd7
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
