# A hosted function named through an alias whose formal stands twice: at its
# argument and at its result's `Try` error row. `H(e)` keeps its layer at every
# use: the argument keeps the declared row while the error row, the hidden
# formal at the end of `H`'s result spine, is the row a use re-opens and widens
# (design.md "Hidden Alias Arguments").
H(e) : e => Try(Str, e)

FallibleEcho := [].{
	# The host returns Err with the tag it was given.
	echo! : H([NotFound, PermissionDenied])
}
