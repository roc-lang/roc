# A lambda that is never called dispatches on a parameter no use pins. The
# parameter's type seals to its uninhabited default, so the dispatch is
# statically unreachable and the program runs in every lowering mode.
ignore = |_f| 0

first : a, b -> a
first = |a, _b| a

main! = |_args| {
	echo!(ignore(|x| x.to_str()).to_str())
	r = { f: |x| x.to_str(), n: 1 }
	echo!(r.n.to_str())
	fs = [|x| x.to_str()]
	echo!(List.len(fs).to_str())
	echo!(first("kept", |x| x.to_str()))
	Ok({})
}
