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
x64mac=b08a7d131d1acd5dd8ff8c7e2f62c7e5c0ea8383f06d0970d9183055ff63b3da
x64win=01641d9c39479e28ec2f49e94f908f162cd5faa44e43af81c015f075ff935648
x64mingw=01641d9c39479e28ec2f49e94f908f162cd5faa44e43af81c015f075ff935648
x64freebsd=8c1dcba8b02969f1aae5299052861450f5f9368dcc62099891afd515567e3a54
x64openbsd=a64296c50f8e8dd7afd0232022926207f1df4c05665d1f12bc894dd739d9cb0a
x64netbsd=2b4a5d71776f9dea119b5eed35eba3c42d54f172190c5e6514a958c8ad64d4b1
x64musl=2b4a5d71776f9dea119b5eed35eba3c42d54f172190c5e6514a958c8ad64d4b1
x64glibc=2b4a5d71776f9dea119b5eed35eba3c42d54f172190c5e6514a958c8ad64d4b1
x64linux=2b4a5d71776f9dea119b5eed35eba3c42d54f172190c5e6514a958c8ad64d4b1
x64elf=2b4a5d71776f9dea119b5eed35eba3c42d54f172190c5e6514a958c8ad64d4b1
x64v1mac=b08a7d131d1acd5dd8ff8c7e2f62c7e5c0ea8383f06d0970d9183055ff63b3da
x64v1win=01641d9c39479e28ec2f49e94f908f162cd5faa44e43af81c015f075ff935648
x64v1mingw=01641d9c39479e28ec2f49e94f908f162cd5faa44e43af81c015f075ff935648
x64v1freebsd=8c1dcba8b02969f1aae5299052861450f5f9368dcc62099891afd515567e3a54
x64v1openbsd=a64296c50f8e8dd7afd0232022926207f1df4c05665d1f12bc894dd739d9cb0a
x64v1netbsd=2b4a5d71776f9dea119b5eed35eba3c42d54f172190c5e6514a958c8ad64d4b1
x64v1musl=2b4a5d71776f9dea119b5eed35eba3c42d54f172190c5e6514a958c8ad64d4b1
x64v1glibc=2b4a5d71776f9dea119b5eed35eba3c42d54f172190c5e6514a958c8ad64d4b1
x64v1linux=2b4a5d71776f9dea119b5eed35eba3c42d54f172190c5e6514a958c8ad64d4b1
x64v1elf=2b4a5d71776f9dea119b5eed35eba3c42d54f172190c5e6514a958c8ad64d4b1
arm64mac=6811e8dc95a70a356714499b69ce680a3011a63e900f3e6a86498ce154fab63c
arm64win=ea189e877f92dfb110ee6cac1a9c9019e5982454bfd7cb4dac0c6491cd5049c9
arm64mingw=ea189e877f92dfb110ee6cac1a9c9019e5982454bfd7cb4dac0c6491cd5049c9
arm64linux=73fc57b26630be333abc7eef7095d72fffda9581b46cee035b0c3be014f4b42d
arm64musl=73fc57b26630be333abc7eef7095d72fffda9581b46cee035b0c3be014f4b42d
arm64glibc=73fc57b26630be333abc7eef7095d72fffda9581b46cee035b0c3be014f4b42d
arm64v1win=ea189e877f92dfb110ee6cac1a9c9019e5982454bfd7cb4dac0c6491cd5049c9
arm64v1mingw=ea189e877f92dfb110ee6cac1a9c9019e5982454bfd7cb4dac0c6491cd5049c9
arm64v1linux=73fc57b26630be333abc7eef7095d72fffda9581b46cee035b0c3be014f4b42d
arm64v1musl=73fc57b26630be333abc7eef7095d72fffda9581b46cee035b0c3be014f4b42d
arm64v1glibc=73fc57b26630be333abc7eef7095d72fffda9581b46cee035b0c3be014f4b42d
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
