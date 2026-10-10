# 128-bit integer to float conversions, driven by a runtime value so the
# operations reach the backends instead of being folded at compile time. The
# operands are past U64 range, so the results depend on the full 128-bit value.
# A default-platform executable links no host that could carry compiler-rt, so
# the compiled program has to convert without it.
main! = |args| {
    count = args.len()

    unsigned : U128
    unsigned = (count.to_u128() + 3) * 100000000000000000000

    signed : I128
    signed = 0 - unsigned.to_i128_wrap()

    echo!("${unsigned.to_f64().to_str()} ${unsigned.to_f32().to_str()} ${signed.to_f64().to_str()} ${signed.to_f32().to_str()}")
    Ok({})
}
