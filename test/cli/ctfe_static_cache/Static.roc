values : List(I64)
values = [7.I64, 11, 17, 23]

divide : I64 -> I64
divide = |denominator| 12.I64 / denominator

Static := [].{
    read : U64 -> I64
    read = |index| {
        var $offset = 0.U64
        var $sum = index.to_i64_wrap()
        while $offset < 8 {
            slot = (index + $offset) % values.len()
            $sum = $sum + (values.get(slot) ?? 0.I64)
            $offset = $offset + 1
        }
        $sum
    }

    guarded : U64 -> I64
    guarded = |index| {
        var $round = 0.U64
        var $sum = 0.I64
        while $round < 3 {
            if index == 0 {
                $sum = $sum + divide(0.I64)
            } else {
                $sum = $sum + Static.read(index)
            }
            $round = $round + 1
        }
        $sum
    }
}
