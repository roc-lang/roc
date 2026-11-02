# repro for https://github.com/roc-lang/roc/issues/12552
# A point-free associated-function alias of a function that calls
# Json.to_str_try must behave the same as its eta-expanded form.
Doc := { n : U64 }.{
	to_json : Doc -> Try(Str, [Infinity, NaN, NegativeInfinity])
	to_json = encode
}

encode : Doc -> Try(Str, [Infinity, NaN, NegativeInfinity])
encode = |doc| Json.to_str_try({ n: doc.n })

expect {
	doc : Doc
	doc = { n: 4 }
	doc.to_json() == Ok("{\"n\":4}")
}
