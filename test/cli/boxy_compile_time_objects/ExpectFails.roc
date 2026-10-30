# A constant failing an inline expect in another module.
import Lib

value : U64
value = Lib.checks(3)

main! = |_args| {
	echo!(Str.inspect(value))
	Ok({})
}
