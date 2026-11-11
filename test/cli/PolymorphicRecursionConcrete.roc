## Recursion at a fixed concrete type reaches finitely many types, so it is
## accepted and specializes under every strategy.
PolymorphicRecursionConcrete(a) := [Val(a)].{
	count : a, U64 -> U64
	count = |_x, n| if n == 0 { 0 } else { 1 + count(1.U8, n - 1) }

	tail : a, U64 -> U64
	tail = |_x, n| if n == 0 { 0 } else { tail(1.U8, n - 1) }

	size : PolymorphicRecursionConcrete(a), U64 -> U64
	size = |_m, n| if n == 0 { 0 } else { 1 + PolymorphicRecursionConcrete.Val(1.U8).size(n - 1) }
}

expect PolymorphicRecursionConcrete.count("s", 3) == 3
expect PolymorphicRecursionConcrete.tail({}, 1) == 0
expect PolymorphicRecursionConcrete.tail("s", 4) == 0
expect PolymorphicRecursionConcrete.Val("s").size(3) == 3
