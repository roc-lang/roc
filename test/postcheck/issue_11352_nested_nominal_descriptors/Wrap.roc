Wrap(a, b) := [Yes(a), No(b)].{
	yes : a -> Wrap(a, b)
	yes = |x| Yes(x)

	no : b -> Wrap(a, b)
	no = |x| No(x)
}
