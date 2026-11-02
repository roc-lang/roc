import Shared

D := [].{}

private_d : I64 -> I64
private_d = |n| if n <= 0 { 0 } else { n + private_d(n - 1) }

make_adder : I64 -> (I64 -> I64)
make_adder = |base| |value| base + value

stored : I64 -> I64
stored = make_adder(3)

expect {
    dbg "D first"
    Shared.check(4) == 14
}

expect {
    dbg "D second"
    expect private_d(2) == 3
    stored(private_d(3)) == 9
}
