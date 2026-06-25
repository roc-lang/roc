app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    Stdout.line!("Hello from Roc WASM!")
    "Hello from Roc WASM!"
}
