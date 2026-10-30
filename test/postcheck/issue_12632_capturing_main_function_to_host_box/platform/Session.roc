Session(a) :: [Reload(a -> Str)].{
	reload : (a -> Str) -> Session(a)
	reload = |render| Reload(render)
}
