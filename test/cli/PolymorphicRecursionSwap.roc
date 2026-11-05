## Recursion that swaps type variables reaches finitely many types, so it is
## accepted and specializes under every strategy.
PolymorphicRecursionSwap := [].{
	go : a, b, U64 -> U64
	go = |x, y, n| if n == 0 { 0 } else { 1 + go(y, x, n - 1) }

	mixed : a, b, U64 -> U64
	mixed = |_x, y, n| if n == 0 { 0 } else { 1 + mixed(y, 1.U8, n - 1) }

	ping : a, b, U64 -> U64
	ping = |x, y, n| if n == 0 { 0 } else { 1 + PolymorphicRecursionSwap.pong(y, x, n - 1) }

	pong : a, b, U64 -> U64
	pong = |x, y, n| if n == 0 { 0 } else { 1 + PolymorphicRecursionSwap.ping(y, x, n - 1) }

	tail : a, b, U64 -> U64
	tail = |x, y, n| if n == 0 { 0 } else { tail(y, x, n - 1) }
}

expect PolymorphicRecursionSwap.go("s", 1.U8, 3) == 3
expect PolymorphicRecursionSwap.mixed("s", {}, 4) == 4
expect PolymorphicRecursionSwap.ping("s", 1.U8, 5) == 5
expect PolymorphicRecursionSwap.tail([1.U16], "s", 5) == 0
