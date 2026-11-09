# A constant crashing in a match branch of another module's code, which every
# strategy reports at the crash and not at the match around it.
import Lib

value : U64
value = Lib.pick(5)

main! = |_args| {
	echo!(Str.inspect(value))
	Ok({})
}
