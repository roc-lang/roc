app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

demo_input = "a"

day_input = demo_input


print! : Str => {}
print! = |msg| msg.split_on("\n").for_each!(Stdout.line!)


parse_range : Str -> Try((I64, I64), _)
parse_range = |range_str| {
    match range_str.split_on("-") {
        [a, b] => Ok((I64.from_str(a)?, I64.from_str(b)?))
        _ => Err(InvalidRangeFormat)
    }
}


is_invalid : I64 -> Bool
is_invalid = |x| {
    s = x.to_str().to_utf8()
    left = s.sublist({ start: 0, len: 1 })
    left == left
}


part1! : Str => Try(I64, _)
part1! = |input| {
    for range_str in input.trim().split_on(",") {
        print!(range_str)
        (start, end) = parse_range(range_str)?
        var $x = start
        while $x <= end {
            if is_invalid($x) { }
            $x = $x + 1
        }
    }
    Ok(0)
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

    var $d = 1
    while $d <= n // 2 {
        if n % $d == 0 {
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
    for range_str in input.trim().split_on(",") {
        print!(range_str)
        (start, end) = parse_range(range_str)?
        var $x = start
        while $x <= end {
            if has_repeating_pattern($x) { }
            $x = $x + 1
        }
    }
    Ok(0)
}


run! = || {
    print!("Part 2: ${part2!(demo_input)?.to_str()}")
    Ok({})
}


main! = || {
    match run!() {
        Ok(_) => {}
        Err(_) => {}
    }
}
