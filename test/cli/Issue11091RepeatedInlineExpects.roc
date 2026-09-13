check = |x| {
    expect x == 1
    expect x < 3
    x
}

expect check(1) + check(2) == 99
