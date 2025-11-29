platform ""
    requires {} { add_ints : I64, I64 -> I64, multiply_ints : I64, I64 -> I64 }
    exposes []
    packages {}
    provides { add_ints_for_host: "add_ints", multiply_ints_for_host: "multiply_ints" }

add_ints_for_host : I64, I64 -> I64
add_ints_for_host = add_ints

multiply_ints_for_host : I64, I64 -> I64
multiply_ints_for_host = multiply_ints
