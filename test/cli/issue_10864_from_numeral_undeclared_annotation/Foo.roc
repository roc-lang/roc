Foo := [Val(I64)].{
	from_numeral : I68 -> Foo
	from_numeral = |n| Foo.Val(n)
}

main = 123.Foo
