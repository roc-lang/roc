# A constant crashing in a module only its import imports.
import Lib

value : U64
value = Lib.through_util(1)

main! = |_args| {
	echo!(Str.inspect(value))
	Ok({})
}
