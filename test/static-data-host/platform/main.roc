platform ""
    requires {} {
        main! : () => {},
        answer : I64,
        flag : Bool,
        flags : List(Bool),
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
        boxed_add_one : Box((I64 -> I64)),
        literal_long : Str,
        assembled_strings : (Str, Str, Str),
        intermediate_final : Str,
        static_slices : (Str, Str),
    }
    exposes []
    packages {}
    provides {
        main_for_host!: "main",
        answer_for_host: "answer",
        flag_for_host: "flag",
        flags_for_host: "flags",
        table_for_host: "table",
        names_for_host: "names",
        tree_for_host: "tree",
        boxed_add_one_for_host: "boxed_add_one",
        literal_long_for_host: "literal_long",
        assembled_strings_for_host: "assembled_strings",
        intermediate_final_for_host: "intermediate_final",
        static_slices_for_host: "static_slices",
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

flag_for_host : Bool
flag_for_host = flag

flags_for_host : List(Bool)
flags_for_host = flags

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

boxed_add_one_for_host : Box((I64 -> I64))
boxed_add_one_for_host = boxed_add_one

literal_long_for_host : Str
literal_long_for_host = literal_long

assembled_strings_for_host : (Str, Str, Str)
assembled_strings_for_host = assembled_strings

intermediate_final_for_host : Str
intermediate_final_for_host = intermediate_final

static_slices_for_host : (Str, Str)
static_slices_for_host = static_slices
