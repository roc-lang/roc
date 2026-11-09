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
x64mac=467ce19babf5c5e25f67b24c3623952db8c6a4301a1ad1568965b495705d9150
x64win=af9b0ce478e2f745f61fca38c516819e6a23c768199e8a74a1e4aacf4fbe07dc
x64mingw=af9b0ce478e2f745f61fca38c516819e6a23c768199e8a74a1e4aacf4fbe07dc
x64freebsd=3889fd6231d76cd1943d9de3693f21ad45cc812661b82ebdb6e9d9eb7513092b
x64openbsd=7a8936c975946f52765946283dc11129ff12574a61a6f2ae18c5f53a64fe7b2a
x64netbsd=59f268983baec292f112ae2149047168e6afbbe6c934367bbe14b5d8a6382eaf
x64musl=59f268983baec292f112ae2149047168e6afbbe6c934367bbe14b5d8a6382eaf
x64glibc=59f268983baec292f112ae2149047168e6afbbe6c934367bbe14b5d8a6382eaf
x64linux=59f268983baec292f112ae2149047168e6afbbe6c934367bbe14b5d8a6382eaf
x64elf=59f268983baec292f112ae2149047168e6afbbe6c934367bbe14b5d8a6382eaf
x64v1mac=467ce19babf5c5e25f67b24c3623952db8c6a4301a1ad1568965b495705d9150
x64v1win=af9b0ce478e2f745f61fca38c516819e6a23c768199e8a74a1e4aacf4fbe07dc
x64v1mingw=af9b0ce478e2f745f61fca38c516819e6a23c768199e8a74a1e4aacf4fbe07dc
x64v1freebsd=3889fd6231d76cd1943d9de3693f21ad45cc812661b82ebdb6e9d9eb7513092b
x64v1openbsd=7a8936c975946f52765946283dc11129ff12574a61a6f2ae18c5f53a64fe7b2a
x64v1netbsd=59f268983baec292f112ae2149047168e6afbbe6c934367bbe14b5d8a6382eaf
x64v1musl=59f268983baec292f112ae2149047168e6afbbe6c934367bbe14b5d8a6382eaf
x64v1glibc=59f268983baec292f112ae2149047168e6afbbe6c934367bbe14b5d8a6382eaf
x64v1linux=59f268983baec292f112ae2149047168e6afbbe6c934367bbe14b5d8a6382eaf
x64v1elf=59f268983baec292f112ae2149047168e6afbbe6c934367bbe14b5d8a6382eaf
arm64mac=6adeac261c6aa64ba81aadcfc5dd0578c8dfb23a001dc94e1a54d04a268c0639
arm64win=7de773207e8708a479ad49e3db6eda4d1e4483bee2f9c98d6fb50861f02f22ec
arm64mingw=7de773207e8708a479ad49e3db6eda4d1e4483bee2f9c98d6fb50861f02f22ec
arm64linux=4bf4a5a439d3563b61f3f521cf29a643b2a27afed2805a543929fb738b62924a
arm64musl=4bf4a5a439d3563b61f3f521cf29a643b2a27afed2805a543929fb738b62924a
arm64glibc=4bf4a5a439d3563b61f3f521cf29a643b2a27afed2805a543929fb738b62924a
arm64v1win=7de773207e8708a479ad49e3db6eda4d1e4483bee2f9c98d6fb50861f02f22ec
arm64v1mingw=7de773207e8708a479ad49e3db6eda4d1e4483bee2f9c98d6fb50861f02f22ec
arm64v1linux=4bf4a5a439d3563b61f3f521cf29a643b2a27afed2805a543929fb738b62924a
arm64v1musl=4bf4a5a439d3563b61f3f521cf29a643b2a27afed2805a543929fb738b62924a
arm64v1glibc=4bf4a5a439d3563b61f3f521cf29a643b2a27afed2805a543929fb738b62924a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
