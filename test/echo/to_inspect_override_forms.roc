# `to_inspect` on a type `T` is a `Str.inspect` override exactly when it can
# be used at `T -> Str`, whatever its form: a lambda or a value alias, each
# annotated or unannotated, on a top-level type and on a function-body type.
# Inspection uses it directly, nested in records and lists, and through
# generic helpers. A generic type's `to_inspect` is used at each application
# it can be used at, such as `Needs(U64)`. A `to_inspect` that cannot be used
# at `T -> Str` is no override: inspection renders the default form, and
# explicit calls still reach the method.

render = |c| "Alias(${c.count.to_str()})"

render_ann = |c| "AliasAnn(${c.count.to_str()})"

render_helper : HelperAnn -> Str
render_helper = |c| "HelperAnn(${c.count.to_str()})"

Lambda := { count : U64 }.{
	to_inspect = |c| "Lambda(${c.count.to_str()})"
}

LambdaAnn := { count : U64 }.{
	to_inspect : LambdaAnn -> Str
	to_inspect = |c| "LambdaAnn(${c.count.to_str()})"
}

Alias := { count : U64 }.{
	to_inspect = render
}

AliasAnn := { count : U64 }.{
	to_inspect : AliasAnn -> Str
	to_inspect = render_ann
}

HelperAnn := { count : U64 }.{
	to_inspect = render_helper
}

NotStr := { count : U64 }.{
	to_inspect = |c| c.count
}

Plain(a) := { inner : a }.{
	to_inspect = |_w| "Plain"
}

Shown(a) := { inner : a }.{
	to_inspect = |w| "Shown(${Str.inspect(w.inner)})"
}

Needs(a) := { inner : a }.{
	to_inspect = |w| "Needs(${w.inner.to_str()})"
}

show = |x| Str.inspect(x)

nested = |x| "${show({ v: x })} ${show([x])}"

main! = |args| {
	n = List.len(args)
	echo!("${Str.inspect(Lambda.{ count: n + 1 })} ${Str.inspect(LambdaAnn.{ count: n + 2 })} ${Str.inspect(Alias.{ count: n + 3 })} ${Str.inspect(AliasAnn.{ count: n + 4 })} ${Str.inspect(HelperAnn.{ count: n + 5 })} ${Str.inspect(NotStr.{ count: n + 6 })}\n")
	echo!("${nested(Lambda.{ count: 1 })} ${nested(LambdaAnn.{ count: 2 })} ${nested(Alias.{ count: 3 })} ${nested(AliasAnn.{ count: 4 })} ${nested(HelperAnn.{ count: 5 })} ${nested(NotStr.{ count: 6 })}\n")

	local_render = |c| "LocalAlias(${c.count.to_str()})"
	local_render_ann = |c| "LocalAliasAnn(${c.count.to_str()})"
	LocalLambda := { count : U64 }.{
		to_inspect = |c| "LocalLambda(${c.count.to_str()})"
	}
	LocalLambdaAnn := { count : U64 }.{
		to_inspect : LocalLambdaAnn -> Str
		to_inspect = |c| "LocalLambdaAnn(${c.count.to_str()})"
	}
	LocalAlias := { count : U64 }.{
		to_inspect = local_render
	}
	LocalAliasAnn := { count : U64 }.{
		to_inspect : LocalAliasAnn -> Str
		to_inspect = local_render_ann
	}
	echo!("${Str.inspect(LocalLambda.{ count: n + 1 })} ${Str.inspect(LocalLambdaAnn.{ count: n + 2 })} ${Str.inspect(LocalAlias.{ count: n + 3 })} ${Str.inspect(LocalAliasAnn.{ count: n + 4 })}\n")
	echo!("${nested(LocalLambda.{ count: 1 })} ${nested(LocalLambdaAnn.{ count: 2 })} ${nested(LocalAlias.{ count: 3 })} ${nested(LocalAliasAnn.{ count: 4 })}\n")
	echo!("${Str.inspect(Plain.{ inner: 1.U8 })} ${Str.inspect(Shown.{ inner: "x" })} ${Str.inspect(Needs.{ inner: 3.U64 })} ${nested(Shown.{ inner: [1.U8] })}\n")
	echo!("${Lambda.{ count: 7 }.to_inspect()} ${Alias.{ count: 8 }.to_inspect()} ${LocalAlias.{ count: 9 }.to_inspect()} ${NotStr.{ count: 10 }.to_inspect().to_str()}\n")
	Ok({})
}
