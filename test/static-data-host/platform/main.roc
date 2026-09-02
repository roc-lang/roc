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
        boxed_static_label : Box((I64 -> Str)),
        literal_long : Str,
        assembled_strings : (Str, Str, Str),
        intermediate_final : Str,
        static_slices : (Str, Str),
    }
    exposes []
    packages {}
    provides {
        "roc_main": main_for_host!,
        "roc_answer": answer_for_host,
        "roc_flag": flag_for_host,
        "roc_flags": flags_for_host,
        "roc_table": table_for_host,
        "roc_names": names_for_host,
        "roc_tree": tree_for_host,
        "roc_boxed_add_one": boxed_add_one_for_host,
        "roc_boxed_static_label": boxed_static_label_for_host,
        "roc_literal_long": literal_long_for_host,
        "roc_assembled_strings": assembled_strings_for_host,
        "roc_intermediate_final": intermediate_final_for_host,
        "roc_static_slices": static_slices_for_host,
    }
    targets: {
        inputs_dir: "targets/",
        x64mac: { inputs: ["libhost.a", app] },
        arm64mac: { inputs: ["libhost.a", app] },
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        x64v1musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64v1musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        x64win: { inputs: ["host.lib", app] },
        arm64win: { inputs: ["host.lib", app] },
        x64mingw: { inputs: ["crt2.obj", "host.lib", app, "libmingw32.lib", "zigc.lib", "compiler_rt.lib", "api-ms-win-crt-conio-l1-1-0.lib", "api-ms-win-crt-convert-l1-1-0.lib", "api-ms-win-crt-environment-l1-1-0.lib", "api-ms-win-crt-filesystem-l1-1-0.lib", "api-ms-win-crt-heap-l1-1-0.lib", "api-ms-win-crt-locale-l1-1-0.lib", "api-ms-win-crt-math-l1-1-0.lib", "api-ms-win-crt-multibyte-l1-1-0.lib", "api-ms-win-crt-private-l1-1-0.lib", "api-ms-win-crt-process-l1-1-0.lib", "api-ms-win-crt-runtime-l1-1-0.lib", "api-ms-win-crt-stdio-l1-1-0.lib", "api-ms-win-crt-string-l1-1-0.lib", "api-ms-win-crt-time-l1-1-0.lib", "api-ms-win-crt-utility-l1-1-0.lib", "advapi32.lib", "kernel32.lib", "ntdll.lib", "shell32.lib", "user32.lib"] },
        arm64mingw: { inputs: ["crt2.obj", "host.lib", app, "libmingw32.lib", "zigc.lib", "compiler_rt.lib", "api-ms-win-crt-conio-l1-1-0.lib", "api-ms-win-crt-convert-l1-1-0.lib", "api-ms-win-crt-environment-l1-1-0.lib", "api-ms-win-crt-filesystem-l1-1-0.lib", "api-ms-win-crt-heap-l1-1-0.lib", "api-ms-win-crt-locale-l1-1-0.lib", "api-ms-win-crt-math-l1-1-0.lib", "api-ms-win-crt-multibyte-l1-1-0.lib", "api-ms-win-crt-private-l1-1-0.lib", "api-ms-win-crt-process-l1-1-0.lib", "api-ms-win-crt-runtime-l1-1-0.lib", "api-ms-win-crt-stdio-l1-1-0.lib", "api-ms-win-crt-string-l1-1-0.lib", "api-ms-win-crt-time-l1-1-0.lib", "api-ms-win-crt-utility-l1-1-0.lib", "advapi32.lib", "kernel32.lib", "ntdll.lib", "shell32.lib", "user32.lib"] },
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

boxed_static_label_for_host : Box((I64 -> Str))
boxed_static_label_for_host = boxed_static_label

literal_long_for_host : Str
literal_long_for_host = literal_long

assembled_strings_for_host : (Str, Str, Str)
assembled_strings_for_host = assembled_strings

intermediate_final_for_host : Str
intermediate_final_for_host = intermediate_final

static_slices_for_host : (Str, Str)
static_slices_for_host = static_slices
