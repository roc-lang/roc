platform ""
    requires { main! : () => {} } {
        answer : I64,
        table : {
            counts: (I64, I64),
            status: [Err(Str), Ok(Str)],
            user: {
                name: Str,
                tags: List(Str),
            },
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
        main_for_host!: "main",
        answer_for_host: "answer",
        table_for_host: "table",
        names_for_host: "names",
        tree_for_host: "tree",
    }
    targets: {
        files: "targets/",
        exe: {
            x64mac: ["libhost.a", app],
            arm64mac: ["libhost.a", app],
            x64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            arm64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            x64win: ["host.lib", app],
            arm64win: ["host.lib", app],
        }
    }

main_for_host! : () => {}
main_for_host! = main!

answer_for_host : I64
answer_for_host = answer

table_for_host : {
    counts: (I64, I64),
    status: [Err(Str), Ok(Str)],
    user: {
        name: Str,
        tags: List(Str),
    },
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
