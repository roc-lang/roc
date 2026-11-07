## Recursion that passes each type variable along unchanged around every
## cycle is accepted and specializes under every strategy, even when its type
## variables trade places across two definitions or one is passed to another
## outside any cycle.
PolymorphicRecursionSwap := [].{
	ping : a, b, U64 -> U64
	ping = |x, y, n| if n == 0 { 0 } else { 1 + PolymorphicRecursionSwap.pong(y, x, n - 1) }

	pong : a, b, U64 -> U64
	pong = |x, y, n| if n == 0 { 0 } else { 1 + PolymorphicRecursionSwap.ping(y, x, n - 1) }

	mixed : a, b, U64 -> U64
	mixed = |_x, y, n| if n == 0 { 0 } else { 1 + mixed(y, 1.U8, n - 1) }
}

expect PolymorphicRecursionSwap.ping("s", 1.U8, 5) == 5
expect PolymorphicRecursionSwap.pong({}, [1.U16], 4) == 4
expect PolymorphicRecursionSwap.mixed("s", {}, 4) == 4
