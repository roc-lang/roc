# A const imported from another module and used at a wider tag union used
# to panic monotype postcheck exactly like the single-module case.
#   postcheck invariant violated: instantiation widened a closed tag union
import Colors

h : [Red, Green, Purple]
h = Colors.f

pick! : List(Str) => [Red, Green, Purple]
pick! = |args| if List.len(args) > 90 Purple else h

main! = |args| {
	match pick!(args) {
		Green => Ok({})
		Red => Err(Exit(1))
		Purple => Err(Exit(2))
	}
}
