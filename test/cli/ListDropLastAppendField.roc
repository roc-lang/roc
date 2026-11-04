# Issue 12799: rebuilding a list with `List.drop_last` and a field of its last
# element. The element borrows the list's storage, so the list cannot be
# shrunk in place while the element is still read.

Repro := [].{
    step = |parts|
        match List.last(parts) {
            Ok(current) => List.append(List.drop_last(parts, 1), { name: current.name })
            Err(_) => parts
        }
}

expect {
    long = "a-string-longer-than-a-small-string"
    out = Repro.step([{ name: Str.concat(long, "") }])

    # Strings of many sizes: one takes the name's memory if that was freed.
    var $others = []
    var $size = 24
    while $size < 200 {
        $others = $others.append(Str.join_with(List.repeat("Z", $size), ""))
        $size = $size + 1
    }

    got = (out.first() ?? { name: "" }).name
    got == long and $others.len() == 176
}

expect {
    long = "a-string-longer-than-a-small-string"
    Repro.step([{ name: long }]).map(|p| p.name) == [long]
}
