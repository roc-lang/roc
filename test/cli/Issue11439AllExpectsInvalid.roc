identity = |x| x

expect identity("a") == 1
expect {
    expect identity("b") == 2
    Bool.True
}
