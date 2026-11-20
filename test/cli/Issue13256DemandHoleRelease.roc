g = |b, xs| (if b { |_| 2 } else { |_| 3 })(List.keep_if(xs, |y| y < 3))

expect g(True, [1]) == 2

expect g(False, [1, 5]) == 3

keep_if_try_arg = |b, xs| (if b { |_| 2 } else { |_| 3 })(List.keep_if_try(xs, |y| Ok(y < 3)))

expect keep_if_try_arg(True, [1]) == 2

expect keep_if_try_arg(False, [1, 5]) == 3

nested_value_call_arg = |b, xs| {
    f = if b { |_| 2 } else { |l| List.len(l) }
    h = if b { |l| l } else { |l| List.append(l, 9) }
    f(h(List.keep_if(xs, |y| y < 3)))
}

expect nested_value_call_arg(True, [1]) == 2

expect nested_value_call_arg(False, [1, 5]) == 2

capturing_variant = |b, xs| {
    n = List.len(xs)
    (if b { |l| List.len(l) + n } else { |_| 3 })(List.keep_if(xs, |y| y < 3))
}

expect capturing_variant(True, [1]) == 2

expect capturing_variant(False, [1, 5]) == 3

two_args = |b, xs| (if b { |a, c| List.len(a) + List.len(c) } else { |_, _| 3 })(List.keep_if(xs, |y| y < 3), List.drop_if(xs, |y| y < 3))

expect two_args(True, [1, 5]) == 2

expect two_args(False, [1, 5]) == 3

two_ignored_args = |b, xs| (if b { |_, _| 2 } else { |_, _| 3 })(List.keep_if(xs, |y| y < 3), List.drop_if(xs, |y| y < 3))

expect two_ignored_args(True, [1]) == 2

expect two_ignored_args(False, [1, 5]) == 3
