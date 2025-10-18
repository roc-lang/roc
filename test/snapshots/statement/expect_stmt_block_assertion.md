# META
~~~ini
description=Debug expression stmt
type=snippet
~~~
# SOURCE
~~~roc
foo : Bool -> Bool
foo = |a| {
    expect a == Bool.True
    a
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwExpect,LowerIdent,OpEquals,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "foo")
			(ty-fn
				(ty (name "Bool"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "foo"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-block
					(statements
						(s-expect
							(e-binop (op "==")
								(e-ident (raw "a"))
								(e-tag (raw "Bool.True"))))
						(e-ident (raw "a"))))))))
~~~
# FORMATTED
~~~roc
foo : Bool -> Bool
foo = |a| {
	expect a == Bool.True
	a
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-block
				(s-expect
					(e-binop (op "eq")
						(e-lookup-local
							(p-assign (ident "a")))
						(e-nominal (nominal "Bool")
							(e-tag (name "True")))))
				(e-lookup-local
					(p-assign (ident "a")))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "Bool") (local))
					(ty-lookup (name "Bool") (local)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool -> Bool")))
	(expressions
		(expr (type "Bool -> Bool"))))
~~~
