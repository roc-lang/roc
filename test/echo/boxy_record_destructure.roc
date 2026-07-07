# Destructuring a record that came back from a generic function exercises the
# dynamic boxed-record path: the record crosses the erased boundary boxed, and
# each destructured field must be extracted with its actual field layout.
identity : a -> a
identity = |x| x

main! = |_args| {
	rec0 : { name : Str, age : U8, score : I64 }
	rec0 = { name: "Bob", age: 25, score: 99 }
	rec = identity(rec0)
	{ name, age, score } = rec
	echo!("${name} ${age.to_str()} ${score.to_str()}")
	Ok({})
}
