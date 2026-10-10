## A rejected method-call argument is reported without making the argument's
## own type, or the method's result type, erroneous for the rest of the
## program: the loop element `id` and the other call of `token` still lower.
Table := { ids : List(U32), counts : List(U64) }.{
    token : Table, U64 -> Str
    token = |_table, n| U64.to_str(n)

    rejected : Table -> Str
    rejected = |table| {
        var $sig = ""
        for id in table.ids {
            $sig = table.token(id)
        }
        $sig
    }

    accepted : Table -> List(Str)
    accepted = |table| {
        var $seen = []
        for count in table.counts {
            $seen = $seen.append(table.token(count))
        }
        $seen
    }
}

expect Table.{ ids: [], counts: [1] }.accepted() == ["1"]
expect Table.{ ids: [2], counts: [] }.rejected() == "2"
