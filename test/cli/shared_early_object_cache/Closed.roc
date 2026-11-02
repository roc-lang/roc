Closed := [].{
    # A loop keeps a real closed procedure under ordinary inlining.
    total : U64 -> U64
    total = |limit| {
        var $index = 0
        var $sum = 0
        while $index < limit {
            $sum = $sum + $index
            $index = $index + 1
        }
        $sum
    }

    # The constant `Err("missing")` is static data whose type mentions the
    # caller's type parameter, so each specialization names its own copy. A
    # caller declared in another module must still find that name after an
    # edit elsewhere in its module.
    first_or_missing : List(a) -> Try(a, Str)
    first_or_missing = |items| {
        match List.first(items) {
            Ok(item) => Ok(item)
            Err(_) => Err("missing")
        }
    }
}
