app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# Repro for https://github.com/roc-lang/roc/issues/10697
#
# summarize({}) is compile-time evaluable, so its list result is stored as a
# packed scalar blob. Both `first` and `second` (kept separate by recursion)
# restore that constant, so LLVM codegen must not reuse the first body's GEP
# instruction when the second body materializes the same backing.
summarize : {} -> { table : List(U64), total : U64 }
summarize = |_| {
    var $table = []
    var $index = 0
    var $total = 0
    while $index < 16 {
        $table = $table.append($index * 7 % 256)
        $total = $total + $index
        $index = $index + 1
    }
    { table: $table, total: $total }
}

table_at : List(U64), U64 -> U64
table_at = |table, n| {
    match table.get(n % table.len()) {
        Err(_) => table.len()
        Ok(value) => value
    }
}

first : U64 -> U64
first = |n| {
    packed = summarize({})
    if n == 0 {
        table_at(packed.table, n) + packed.total
    } else {
        first(n - 1) + table_at(packed.table, n) + 1
    }
}

second : U64 -> U64
second = |n| {
    packed = summarize({})
    if n == 0 {
        table_at(packed.table, n) + packed.total
    } else {
        second(n - 1) + table_at(packed.table, n) + 2
    }
}

main! = || {
    n = match Stdin.line!() {
        "three" => 3
        _ => 0
    }
    Stdout.line!(first(n).to_str())
    Stdout.line!(second(n).to_str())
}
