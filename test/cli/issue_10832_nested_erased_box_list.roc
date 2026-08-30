# Repro for https://github.com/roc-lang/roc/issues/10832: an unresolved
# Box of a closure nested in a list element still makes the element refcounted.
app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout

Ev : { code : U64 }

Rt(model) := {
	at : U32,
	handler : [NoHandler, On(Box((model, Ev -> model)))],
}.{
	new : U32 -> Rt(model)
	new = |at| { at, handler: NoHandler }
}

build : List(U32) -> Str
build = |xs| {
	shadow = List.map(xs, |x| Rt.new(x))
	if List.len(shadow) > 1000000 { "big" } else { "small" }
}

main! = || {
	xs = List.repeat(3.U32, 1)
	Stdout.line!(build(xs))
}
