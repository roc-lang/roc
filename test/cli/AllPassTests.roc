AllPassTests := {}

add = |a, b| a + b

double = |x| x * 2

expect {
    add(1, 2) == 3
}

expect {
    add(0, 0) == 0
}

expect {
    double(5) == 10
}
