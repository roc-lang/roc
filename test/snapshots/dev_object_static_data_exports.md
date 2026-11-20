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
x64mac=900bf97b7715797ff0faa26ba207fb2e56b2047685666ab030696c01ee6be03a
x64win=36e1e8462054c64cdbedd930af0fd045acb7c61e1ed4e083fc0de34d605f3da2
x64mingw=36e1e8462054c64cdbedd930af0fd045acb7c61e1ed4e083fc0de34d605f3da2
x64freebsd=8e09757dfef6405dc029b64f4ee3c606e70aafbabddf076c6398212da34336e7
x64openbsd=d5c7385532e8f5fa41b0d513c7f4bc981c35e68b312fdb86e3737c8c4f775406
x64netbsd=293d21752c9e5ac6c6e895c5846831485f2a078b651b9b1dbe903424bd3268b4
x64musl=293d21752c9e5ac6c6e895c5846831485f2a078b651b9b1dbe903424bd3268b4
x64glibc=293d21752c9e5ac6c6e895c5846831485f2a078b651b9b1dbe903424bd3268b4
x64linux=293d21752c9e5ac6c6e895c5846831485f2a078b651b9b1dbe903424bd3268b4
x64elf=293d21752c9e5ac6c6e895c5846831485f2a078b651b9b1dbe903424bd3268b4
x64v1mac=900bf97b7715797ff0faa26ba207fb2e56b2047685666ab030696c01ee6be03a
x64v1win=36e1e8462054c64cdbedd930af0fd045acb7c61e1ed4e083fc0de34d605f3da2
x64v1mingw=36e1e8462054c64cdbedd930af0fd045acb7c61e1ed4e083fc0de34d605f3da2
x64v1freebsd=8e09757dfef6405dc029b64f4ee3c606e70aafbabddf076c6398212da34336e7
x64v1openbsd=d5c7385532e8f5fa41b0d513c7f4bc981c35e68b312fdb86e3737c8c4f775406
x64v1netbsd=293d21752c9e5ac6c6e895c5846831485f2a078b651b9b1dbe903424bd3268b4
x64v1musl=293d21752c9e5ac6c6e895c5846831485f2a078b651b9b1dbe903424bd3268b4
x64v1glibc=293d21752c9e5ac6c6e895c5846831485f2a078b651b9b1dbe903424bd3268b4
x64v1linux=293d21752c9e5ac6c6e895c5846831485f2a078b651b9b1dbe903424bd3268b4
x64v1elf=293d21752c9e5ac6c6e895c5846831485f2a078b651b9b1dbe903424bd3268b4
arm64mac=040b636076255befed5fa9d4a4b75b2d3c3ade9dff3fe596fd8c1b600b1b5e37
arm64win=a451ee37696b5b9ed47a7bafbab6d4217ea0f561a371e86d5301497012b140f1
arm64mingw=a451ee37696b5b9ed47a7bafbab6d4217ea0f561a371e86d5301497012b140f1
arm64linux=a317af2be9dc7df2ca28b6191277a8fadda2b636cbf2ebf74ca1d2ca2f9fe3bd
arm64musl=a317af2be9dc7df2ca28b6191277a8fadda2b636cbf2ebf74ca1d2ca2f9fe3bd
arm64glibc=a317af2be9dc7df2ca28b6191277a8fadda2b636cbf2ebf74ca1d2ca2f9fe3bd
arm64v1win=a451ee37696b5b9ed47a7bafbab6d4217ea0f561a371e86d5301497012b140f1
arm64v1mingw=a451ee37696b5b9ed47a7bafbab6d4217ea0f561a371e86d5301497012b140f1
arm64v1linux=a317af2be9dc7df2ca28b6191277a8fadda2b636cbf2ebf74ca1d2ca2f9fe3bd
arm64v1musl=a317af2be9dc7df2ca28b6191277a8fadda2b636cbf2ebf74ca1d2ca2f9fe3bd
arm64v1glibc=a317af2be9dc7df2ca28b6191277a8fadda2b636cbf2ebf74ca1d2ca2f9fe3bd
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
