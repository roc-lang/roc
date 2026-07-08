app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import "static_bytes_asset.txt" as asset : List(U8)

expect List.len(asset) == 6

main! : () => {}
main! = ||
    Stdout.line!("bytes: ${U64.to_str(List.len(asset))}")
