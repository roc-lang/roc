app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Issue 13376: all three loops own the record and overwrite an existing key.
Item : { name : Str, value : U64 }
Table : { rows : Dict(U64, Item), other : Str, version : U64 }

put_let : Table, U64, Item -> Table
put_let = |t, key, row| {
        { rows, other, version } = t
        { rows: rows.insert(key, row), other, version: version + 1 }
}

put_param : Table, U64, Item -> Table
put_param = |{ rows, other, version }, key, row| { rows: rows.insert(key, row), other, version: version + 1 }

put_match : Table, U64, Item -> Table
put_match = |t, key, row| match t {
        { rows, other, version } => { rows: rows.insert(key, row), other, version: version + 1 }
}

three_let : Table -> Table
three_let = |table| {
        var $t = table
        for id in 0..<3.U64 {
                $t = put_let($t, id, { name: "x", value: id })
        }
        $t
}

three_param : Table -> Table
three_param = |table| {
        var $t = table
        for id in 0..<3.U64 {
                $t = put_param($t, id, { name: "x", value: id })
        }
        $t
}

three_match : Table -> Table
three_match = |table| {
        var $t = table
        for id in 0..<3.U64 {
                $t = put_match($t, id, { name: "x", value: id })
        }
        $t
}

make : U64 -> Table
make = |n| {
        var $rows = Dict.with_capacity(n)
        for i in 0..<n {
                $rows = $rows.insert(i, { name: "row", value: i })
        }
        { rows: $rows, other: "other", version: 0 }
}

run! : Str => Str
run! = |input| {
        n = 3 + input.count_utf8_bytes()
        a = make(n)
        b = make(n)
        c = make(n)
        start = Host.alloc_count!()
        ra = three_let(a)
        mid = Host.alloc_count!()
        rb = three_param(b)
        end_param = Host.alloc_count!()
        rc = three_match(c)
        end_match = Host.alloc_count!()
        # Keep each loop out of its caller, as in the report.
        again = three_let(ra).version + three_param(rb).version + three_match(rc).version
        "versions ${again.to_str()}; allocations: statement ${(mid - start).to_str()}, parameter ${(end_param - mid).to_str()}, match ${(end_match - end_param).to_str()}"
}
