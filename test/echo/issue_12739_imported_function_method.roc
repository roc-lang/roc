# repro for https://github.com/roc-lang/roc/issues/12739
# Methods bound directly to functions from another module are real methods:
# Str.inspect uses the imported to_inspect, == uses the imported is_eq, and
# an explicit call runs the imported function, printing "custom False custom".
import Issue12739Other

Rows := { n : U8 }.{
	to_inspect = Issue12739Other.render
	is_eq = Issue12739Other.same
}

main! = |_args| {
	x = Rows.({ n: 1 })
	echo!("${Str.inspect(x)} ${Str.inspect(x == x)} ${x.to_inspect()}\n")
	Ok({})
}
