import Hidden

Public := [].{
	Crate(x) : Hidden.Crate(x)

	get : Crate(x) -> Crate(x)
	get = |crate| Hidden.Crate.get(crate)
}
