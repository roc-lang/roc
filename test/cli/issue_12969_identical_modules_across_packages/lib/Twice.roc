import S

Twice := [].{
	twice : Str -> Str
	twice = |s| S.wrap(S.wrap(s))
}
