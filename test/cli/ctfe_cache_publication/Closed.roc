Closed := [].{
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

    checked_scale : U64 -> U64
    checked_scale = |scale| {
        var $index = 0
        var $scaled = 0
        while $index < 10 {
            $scaled = $scaled + scale
            $index = $index + 1
        }
        $scaled
    }

    # This export is never needed while checking the app.
    runtime_total : U64 -> U64
    runtime_total = |limit| {
        var $index = 0
        var $sum = 100
        while $index < limit {
            $sum = $sum + $index
            $index = $index + 1
        }
        $sum
    }
}
