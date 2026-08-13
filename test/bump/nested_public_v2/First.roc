import Second

First :: [].{
	Foo := [A].{
		to_bar : Foo -> Second.Bar
		to_bar = |_| X
	}
}
