app [main!] { pf: platform "./static-lib-platform/main.roc" }

# More bytes than a 32-bit address space holds: evaluating this constant for a
# wasm32 build runs out of memory where `Str.repeat` asks for them.
big : Str
big = Str.repeat("ab", 4294967297)

main! = |_seed| Str.inspect(Str.count_utf8_bytes(big))
