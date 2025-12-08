platform ""
    requires {} { process_string : Str -> Str }
    exposes []
    packages {}
    provides { process_string_for_host: "process_string" }
    targets: {
        files: "targets/",
        exe: {
            x64mac: ["libhost.a", app],
            arm64mac: ["libhost.a", app],
            x64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            arm64musl: ["crt1.o", "libhost.a", app, "libc.a"],
            x64glibc: ["libhost.a", app],
            arm64glibc: ["libhost.a", app],
        }
    }

process_string_for_host : Str -> Str
process_string_for_host = process_string
