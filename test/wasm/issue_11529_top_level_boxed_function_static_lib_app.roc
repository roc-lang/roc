app [main!] { pf: platform "./platform/main.roc" }

# repro for https://github.com/roc-lang/roc/issues/11529
# A referenced top-level constant containing a boxed function must compile for
# wasm32 from an empty cache and preserve the function's behavior.

handler : Box(Str -> Str)
handler = Box.box(|key| key)

main! = || {
    f = Box.unbox(handler)
    f("x")
}
