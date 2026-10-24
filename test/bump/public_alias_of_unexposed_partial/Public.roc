import Hidden

Public := [].{
	StrCrate : Hidden.Crate(Str)

	get : StrCrate -> StrCrate
	get = |crate| Hidden.Crate.get(crate)
}
