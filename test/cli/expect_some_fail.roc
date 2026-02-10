add = |a, b| a + b

expect {
    add(1, 2) == 3
}

expect {
    add(1, 1) == 3
}

expect {
    add(2, 2) == 4
}
