FailWithDocComment := {}

add = |a, b| a + b

## This test should fail
expect {
    add(1, 1) == 3
}

expect {
    add(2, 2) == 4
}
