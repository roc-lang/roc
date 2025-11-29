app [add_ints, multiply_ints] { pf: platform "./platform/main.roc" }

add_ints : I64, I64 -> I64
add_ints = |a, b| a + b

multiply_ints : I64, I64 -> I64
multiply_ints = |a, b| a * b
