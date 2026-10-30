# A constant crashing inside another module's code, which has to report the
# crash's own location whatever the cache holds.
import Lib

boom : U64
boom = Lib.explode(3)

main! = |_args| {
	echo!(Str.inspect(boom))
	Ok({})
}
