import Shared

A := [].{}

private_a : I64 -> I64
private_a = |n| if n <= 0 { 0 } else { n + private_a(n - 1) }

make_adder : I64 -> (I64 -> I64)
make_adder = |base| |value| base + value

stored : I64 -> I64
stored = make_adder(3)

expect {
    dbg "A first"
    Shared.check(1) == 11
}

expect {
    dbg "A second"
    expect private_a(2) == 3
    stored(private_a(3)) == 9
}
