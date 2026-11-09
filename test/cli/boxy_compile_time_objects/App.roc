# Compile-time evaluation of this module calls `Lib` and Builtin through their
# cached objects.
import Lib

answer : U64
answer = Lib.compute(10) + Lib.names.len()

text : Str
text = "${Str.inspect(answer)} ${Str.inspect([1.U8, 2].map(|x| x + 1))}"

main! = |_args| {
	echo!(text)
	Ok({})
}
