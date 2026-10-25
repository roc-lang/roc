import Shared

B := [].{}

private_b : I64 -> I64
private_b = |n| if n <= 0 { 0 } else { n + private_b(n - 1) }

make_adder : I64 -> (I64 -> I64)
make_adder = |base| |value| base + value

stored : I64 -> I64
stored = make_adder(3)

expect {
    dbg "B first"
    Shared.check(2) == 12
}

expect {
    dbg "B second"
    expect private_b(2) == 3
    stored(private_b(3)) == 9
}
