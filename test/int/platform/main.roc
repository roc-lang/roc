platform ""
    requires {} { add_ints : I64, I64 -> I64, multiply_ints : I64, I64 -> I64 }
    exposes []
    packages {}
    provides { add_ints_for_host: "add_ints", multiply_ints_for_host: "multiply_ints" }
    targets: {
        exe: {
            x64mac: [app],
            arm64mac: [app],
            x64musl: [app],
            arm64musl: [app],
        }
    }

add_ints_for_host : I64, I64 -> I64
add_ints_for_host = add_ints

multiply_ints_for_host : I64, I64 -> I64
multiply_ints_for_host = multiply_ints
