# The expect sites in this function should be reported as tests when square runs.







square = |x| {
    expect 2 * 2 == 5
    expect 3 * 3 == 10
    x * x
}

expect square(2) == 5
