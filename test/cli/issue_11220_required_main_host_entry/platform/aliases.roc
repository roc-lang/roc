platform ""
    requires {} { main : Str -> Str }
    exposes []
    packages {}
    provides { "another_string_export": process_string_for_host, "roc_process_string": process_string_for_host }
    targets: {
        inputs_dir: "../../../str/platform/targets/",
        x64mac: { inputs: ["libhost.a", app] },
        arm64mac: { inputs: ["libhost.a", app] },
        x64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        x64v1musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        arm64v1musl: { inputs: ["crt1.o", "libhost.a", app, "libc.a"] },
        x64glibc: { inputs: ["Scrt1.o", "crti.o", "libhost.a", app, "crtn.o", "libc.so"] },
        arm64glibc: { inputs: ["Scrt1.o", "crti.o", "libhost.a", app, "crtn.o", "libc.so"] },
    }

process_string_for_host : Str -> Str
process_string_for_host = |input| main(input)
