# A generalized const (implicit polarity row extension) used at a wider tag
# union used to panic monotype postcheck at the deferred const-use boundary:
#   postcheck invariant violated: instantiation widened a closed tag union
f : [Red, Green]
f = Green

h : [Red, Green, Purple]
h = f

pick! : List(Str) => [Red, Green, Purple]
pick! = |args| if List.len(args) > 90 Purple else h

main! = |args| {
	code = match pick!(args) {
		Red => 1
		Green => 2
		Purple => 3
	}
	if code == 2 Ok({}) else Err(Exit(code))
}
