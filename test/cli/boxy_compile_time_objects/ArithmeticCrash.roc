# A constant overflowing in compiled arithmetic in another module.
import Lib

value : U8
value = Lib.narrow(100)

main! = |_args| {
	echo!(Str.inspect(value))
	Ok({})
}
