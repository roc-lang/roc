Hidden :: [].{
	Crate(a) :: { item : a }.{
		get : Crate(a) -> Crate(a)
		get = |crate| crate

		peek : Crate(a) -> Crate(a)
		peek = |crate| crate
	}
}
