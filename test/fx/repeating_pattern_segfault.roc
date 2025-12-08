app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

demo_input = "11-22"


print! : Str => {}
print! = |msg| msg.split_on("\n").for_each!(Stdout.line!)


parse_range : Str -> Try((I64, I64), _)
parse_range = |range_str| {
    match range_str.split_on("-") {
        [a, b] => Ok((I64.from_str(a)?, I64.from_str(b)?))
        _ => Err(InvalidRangeFormat)
    }
}


repeat = |list, n| repeat_helper([], list, n)

repeat_helper = |acc, list, n| match n {
    0 => acc
    _ => repeat_helper(acc.concat(list), list, n - 1)
}

has_repeating_pattern : I64 => Bool
has_repeating_pattern = |x| {
    s = x.to_str().to_utf8()
    n = s.len()

    # Check all divisors of n
    var $d = 1
    while $d <= n // 2 {
        if n % $d == 0 {
            # Check if repeating the first d characters n/d times equals s
            slice = s.sublist({ start: 0, len: $d })
            repeated = slice->repeat(n // $d)
            if repeated == s { return True }
        }
        $d = $d + 1
    }

    False
}


part2! : Str => Try(I64, _)
part2! = |input| {
    var $sum = 0

    for range_str in input.trim().split_on(",") {
        print!(range_str)
        (start, end) = parse_range(range_str)?

        var $x = start
        while $x <= end {
            if has_repeating_pattern($x) {
                $sum = $sum + $x
            }
            $x = $x + 1
        }
    }

    Ok($sum)
}


run! = || {
    print!("Part 2 (demo): ${part2!(demo_input.trim())?.to_str()}")
    Ok({})
}


main! = || {
    match run!() {
        Ok(_) => {}
        Err(_) => {}
    }
}
