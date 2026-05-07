MissingMethodDoesNotPanic :: [].{
	foo : List(U8) -> List(U8)
	foo = |list| list.reverse()
}

expect foo([1, 2, 3]) == [3, 2, 1]
