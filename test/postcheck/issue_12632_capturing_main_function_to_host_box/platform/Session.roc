Session(a) :: [Reload(Box((a -> Str)))].{
	reload : (a -> Str) -> Session(a)
	reload = |render| Reload(Box.box(render))
}
