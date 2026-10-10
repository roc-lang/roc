# A method of a type declared in an inner block of an annotated function names
# that function's type variable, so it captures no value but is not promoted.
# A value of the type leaves the inner block and its method is still called,
# directly and through a local generic helper, at each instantiation.

count_with : List(a) -> U64
count_with = |xs| {
	w = {
		W := { n : U64 }.{
			get = |v| {
				empty : List(a)
				empty = []
				v.n + List.len(empty) + 1
			}
		}
		W.{ n: List.len(xs) }
	}
	read = |v| v.get()
	w.get() + read(w)
}

main! = |_args| {
	echo!("${count_with([1, 2]).to_str()} ${count_with(["a"]).to_str()}\n")
	Ok({})
}
