import pf.Stdout

names : List(Str)
names = ["Alice", "Bob"].map(|a| a)

adder : I64 -> (I64 -> I64)
adder = |n| |x| x + n

handlers : List(I64 -> I64)
handlers = [adder(2)]

expect names == ["Alice", "Bob"]
expect match handlers {
	[f] => f(3) == 5
	_ => False
}
