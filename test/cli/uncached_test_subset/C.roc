import Shared

C := [].{}

private_c : I64 -> I64
private_c = |n| if n <= 0 { 0 } else { n + private_c(n - 1) }

make_adder : I64 -> (I64 -> I64)
make_adder = |base| |value| base + value

stored : I64 -> I64
stored = make_adder(3)

expect {
    dbg "C first"
    Shared.check(3) == 13
}

expect {
    dbg "C second"
    expect private_c(2) == 4
    stored(private_c(3)) == 9
}
