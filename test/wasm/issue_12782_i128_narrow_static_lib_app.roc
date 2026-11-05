app [main!] { pf: platform "./static-lib-platform/main.roc" }

# Narrowing `_try` conversions from I128 and U128 to 8- and 16-bit integers
# (#12782). The seed comes from the host so that no conversion is evaluated
# at compile time.

main! = |seed| {
    n : I128
    n = seed.to_i128() + 3
    neg = 0 - n
    big = n + 297
    un : U128
    un = seed.to_u128() + 3
    "u8=${Str.inspect(n.to_u8_try())} u16=${Str.inspect(n.to_u16_try())} i8=${Str.inspect(n.to_i8_try())} i16=${Str.inspect(n.to_i16_try())} u32=${Str.inspect(n.to_u32_try())} i64=${Str.inspect(n.to_i64_try())} neg_u8=${Str.inspect(neg.to_u8_try())} neg_i8=${Str.inspect(neg.to_i8_try())} big_u8=${Str.inspect(big.to_u8_try())} big_u16=${Str.inspect(big.to_u16_try())} un_u8=${Str.inspect(un.to_u8_try())} un_i16=${Str.inspect(un.to_i16_try())}"
}
