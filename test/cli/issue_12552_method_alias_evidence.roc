# repro for https://github.com/roc-lang/roc/issues/12552
# Methods bound point-free to a procedure whose JSON encoding needs codec
# evidence from its callers, called as methods directly and through `where`
# requirements, at the top level and in a block.
Doc := { n : U64 }.{
	to_json : Doc -> Try(Str, err)
	to_json = encode_doc
}

encode_doc : Doc -> Try(Str, err)
encode_doc = |doc| Json.to_str_try({ n: doc.n })

Wrap(a) := { v : a }.{
	to_json = encode_wrap
	again = to_json
}

encode_wrap = |w| Json.to_str_try({ v: w.v })

show : x -> Try(Str, e) where [x.to_json : x -> Try(Str, e)]
show = |x| x.to_json()

expect {
	doc : Doc
	doc = { n: 4 }
	doc.to_json() == Ok("{\"n\":4}")
}

expect {
	w : Wrap(U64)
	w = { v: 4 }
	w.to_json() == Ok("{\"v\":4}")
}

expect {
	w : Wrap(U64)
	w = { v: 4 }
	w.again() == Ok("{\"v\":4}")
}

expect {
	w : Wrap(Str)
	w = { v: "hi" }
	show(w) == Ok("{\"v\":\"hi\"}")
}

expect {
	Local(a) := { v : a }.{
		to_json = encode_wrap
	}
	n : Local(U64)
	n = { v: 4 }
	s : Local(Str)
	s = { v: "s" }
	n.to_json() == Ok("{\"v\":4}") and show(s) == Ok("{\"v\":\"s\"}")
}
