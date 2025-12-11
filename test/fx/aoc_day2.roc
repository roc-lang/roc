app [main!] { pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.6/2BfGn4M9uWJNhDVeMghGeXNVDFijMfPsmmVeo6M4QjKX.tar.zst" }

import pf.Stdout

demo_input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
#day_input = demo_input # TODO: copy actual input here
day_input = "67562556-67743658,62064792-62301480,4394592-4512674,3308-4582,69552998-69828126,9123-12332,1095-1358,23-48,294-400,3511416-3689352,1007333-1150296,2929221721-2929361280,309711-443410,2131524-2335082,81867-97148,9574291560-9574498524,648635477-648670391,1-18,5735-8423,58-72,538-812,698652479-698760276,727833-843820,15609927-15646018,1491-1766,53435-76187,196475-300384,852101-903928,73-97,1894-2622,58406664-58466933,6767640219-6767697605,523453-569572,7979723815-7979848548,149-216"

main! = |_args| {
    match run!() {
        Ok(_) => Ok({})
        Err(_) => Err(1)
    }
}

run! = || {
    part1_demo = part1!(demo_input)?
    Stdout.line!("-----------")
    part1_full = part1!(day_input)?
    Stdout.line!("-----------")
    part2_demo = part2!(demo_input)?
    Stdout.line!("-----------")
    part2_full = part2!(day_input)?
    Stdout.line!("-----------")
    Stdout.line!("Part 1 (demo): ${part1_demo.to_str()}")
    Stdout.line!("Part 1: ${part1_full.to_str()}")
    Stdout.line!("Part 2 (demo): ${part2_demo.to_str()}")
    Stdout.line!("Part 2: ${part2_full.to_str()}")
    Ok({})
}

part1! : Str => Try(U64, _)
part1! = |input| {
    var $all_invalid = []

    for range_str in input.trim().split_on(",") {
        #Stdout.line!(range_str)
        range = parse_range(range_str)?
        var $range_invalid = []

        # Split by digit count and process each sub-range
        sub_ranges = split_by_digit_count(range)
        for sub_range in sub_ranges {
            invalid = find_invalid_in_range_part1(sub_range)
            $range_invalid = $range_invalid.concat(invalid)
        }

        # Accumulate all invalid numbers
        unique = dedup($range_invalid)
        $all_invalid = $all_invalid.concat(unique)
    }

    sum = $all_invalid.fold(0, |acc, n| acc + n)
    Ok(sum)
}

part2! : Str => Try(U64, _)
part2! = |input| {
    var $all_invalid = []

    for range_str in input.trim().split_on(",") {
        #Stdout.line!(range_str)
        range = parse_range(range_str)?
        var $range_invalid = []

        # Split by digit count and process each sub-range
        sub_ranges = split_by_digit_count(range)
        for sub_range in sub_ranges {
            invalid = find_invalid_in_range_part2(sub_range)
            $range_invalid = $range_invalid.concat(invalid)
        }

        # Accumulate all invalid numbers
        unique = dedup($range_invalid)
        $all_invalid = $all_invalid.concat(unique)
    }

    sum = $all_invalid.fold(0, |acc, n| acc + n)
    Ok(sum)
}

# Ideally we use a Set, but it's not available yet
# Next ideally we sort the list, but it's not clear to me how sort_with works
# So we just check if any duplicate exists.
dedup = |list| {
    var $deduped = []
    for item in list {
        if !$deduped.contains(item) {
            $deduped = $deduped.append(item)
        }
    }
    $deduped
}

parse_range : Str -> Try((U64, U64), _)
parse_range = |range_str| {
    match range_str.split_on("-") {
        [a, b] => Ok((U64.from_str(a)?, U64.from_str(b)?))
        _ => Err(InvalidRangeFormat)
    }
}

# Split a range into sub-ranges where all numbers have the same digit count
split_by_digit_count : (U64, U64) -> List((U64, U64))
split_by_digit_count = |(start, end)| {
    start_digits = count_digits(start)
    end_digits = count_digits(end)

    if start_digits == end_digits {
        [(start, end)]
    } else {
        # Find the boundary - the largest number with start_digits digits
        boundary = pow(10, start_digits) - 1
        first_range = (start, boundary)
        split_by_digit_count((boundary + 1, end)).append(first_range)
    }
}

# Count digits in a number
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

# Helper: compute base^exp
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

# Find all invalid numbers in a single-digit-count range
# In part 1, it's all numbers that are exactly the repetition twice of a slice
find_invalid_in_range_part1 : (U64, U64) -> List(U64)
find_invalid_in_range_part1 = |(start, end)| {
    digit_count = count_digits(start)
    divisors = if digit_count % 2 == 0 { [digit_count // 2] } else { [] }

    var $all_invalid = []
    for divisor in divisors {
        invalid_for_divisor = generate_repeating(divisor, digit_count, start, end)
        $all_invalid = $all_invalid.concat(invalid_for_divisor)
    }

    $all_invalid
}

# Find all invalid numbers in a single-digit-count range
# In part 2, it's all numbers that display a repeating pattern of digits
find_invalid_in_range_part2 : (U64, U64) -> List(U64)
find_invalid_in_range_part2 = |(start, end)| {
    digit_count = count_digits(start)
    divisors = get_divisors(digit_count)

    var $all_invalid = []
    for divisor in divisors {
        invalid_for_divisor = generate_repeating(divisor, digit_count, start, end)
        $all_invalid = $all_invalid.concat(invalid_for_divisor)
    }

    $all_invalid
}

# Get all proper divisors of n (excluding n itself)
get_divisors : U64 -> List(U64)
get_divisors = |n| {
    var $divisors = []
    var $d = 1
    while $d <= n // 2 {
        if n % $d == 0 {
            $divisors = $divisors.append($d)
        }
        $d = $d + 1
    }
    $divisors
}

# Generate all repeating numbers with given pattern length within range
generate_repeating : U64, U64, U64, U64 -> List(U64)
generate_repeating = |pattern_len, digit_count, range_start, range_end| {
    # Compute the multiplier for converting pattern to repeated number
    # For pattern P of length D repeated R times:
    # result = P * (10^(D*(R-1)) + 10^(D*(R-2)) + ... + 10^D + 1)
    # This geometric series sum = (10^(D*R) - 1) / (10^D - 1)
    numerator = pow(10, digit_count) - 1
    denominator = pow(10, pattern_len) - 1
    multiplier = numerator // denominator

    # Pattern ranges from 10^(pattern_len-1) to 10^pattern_len - 1
    # to ensure it has exactly pattern_len digits
    # If pattern_len = 2, range_start = 123456, and range_end = 567123
    # we may start with pattern_min = 12, pattern_max = 56
    # 12 == 123456 // 10000
    # 56 == 567123 // 10000
    pattern_min = range_start // pow(10, digit_count - pattern_len)
    pattern_max = range_end // pow(10, digit_count - pattern_len)

    var $results = []
    var $pattern = pattern_min
    while $pattern <= pattern_max {
        repeated = $pattern * multiplier
        if range_start <= repeated and repeated <= range_end {
            $results = $results.append(repeated)
        }
        $pattern = $pattern + 1
    }

    $results
}
