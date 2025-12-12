app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = || {
    result = split_by_digit_count((1, 1000))
    Stdout.line!("Result count: ${result.len().to_str()}")
}

split_by_digit_count : (U64, U64) -> List((U64, U64))
split_by_digit_count = |(start, end)| {
    start_digits = count_digits(start)
    end_digits = count_digits(end)

    if start_digits == end_digits {
        [(start, end)]
    } else {
        boundary = pow(10, start_digits) - 1
        first_range = (start, boundary)
        split_by_digit_count((boundary + 1, end)).append(first_range)
    }
}

count_digits : U64 -> U64
count_digits = |n| {
    if n == 0 { return 1 }
    var $count = 0
    var $num = n
    while $num > 0 {
        $count = $count + 1
        $num = $num // 10
    }
    $count
}

pow : U64, U64 -> U64
pow = |base, exp| {
    if exp == 0 {
        1
    } else {
        var $result = 1
        var $b = base
        var $e = exp
        while $e > 0 {
            if $e % 2 == 1 {
                $result = $result * $b
            }
            $b = $b * $b
            $e = $e // 2
        }
        $result
    }
}
