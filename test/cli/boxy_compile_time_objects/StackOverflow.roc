# A constant recursing forever in another module.
import Lib

value : U64
value = Lib.forever(0)

main! = |_args| {
	echo!(Str.inspect(value))
	Ok({})
}
