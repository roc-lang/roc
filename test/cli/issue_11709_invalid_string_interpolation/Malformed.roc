# Each value's expression contains the reserved word `module` where an
# expression should be. The parser reports each one exactly once; the
# surrounding expressions keep their shape instead of dropping the broken part.
Malformed := [].{
	list = [module]
	call = Str.concat("a", module)
	sum = 1 + module
	bare = module
	record = { a: module }
	guard = match 1 {
		1 if module => 2
		_ => 3
	}
}
