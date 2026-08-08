Generic :: [].{
	contains : List(a), a -> Bool where [a.is_eq : a, a -> Bool]
	contains = |items, target|
		match items {
			[first, .. as rest] =>
				if first == target {
					True
				} else {
					contains(rest, target)
				}
			[] => False
		}
}
