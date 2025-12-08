platform ""
    requires {} { add_ints : I64, I64 -> I64, multiply_ints : I64, I64 -> I64 }
    exposes []
    packages {}
    provides { add_ints_for_host: "add_ints", multiply_ints_for_host: "multiply_ints" }
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

add_ints_for_host : I64, I64 -> I64
add_ints_for_host = add_ints

multiply_ints_for_host : I64, I64 -> I64
multiply_ints_for_host = multiply_ints
