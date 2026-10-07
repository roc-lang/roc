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
}
