app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import "../../CONTRIBUTING/profiling/bench_repeated_check_ORIGINAL.roc" as data : Str

main! = || {
    byte_count = Str.count_utf8_bytes(data)
    Stdout.line!("bytes: ${Str.inspect(byte_count)}")
}
