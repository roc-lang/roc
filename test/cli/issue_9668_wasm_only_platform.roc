app [main!] { pf: platform "../wasm/static-lib-platform/main.roc" }

main! = |_seed| "ok"

# A platform's build targets do not affect pure tests, which run natively.
expect 1 == 1
