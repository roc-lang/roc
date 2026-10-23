Hidden :: [].{
	Crate(a) :: { item : a }.{
		get : Crate(a) -> Crate(a)
		get = |crate| crate
	}
}
