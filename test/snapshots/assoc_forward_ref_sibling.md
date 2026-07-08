# META
~~~ini
description=A forward reference from one associated item to a later sibling in the same block stays legal
type=file:Fwd.roc
~~~
# SOURCE
~~~roc
Fwd := [].{
    first = second
    second = 42
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Fwd")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-decl
					(p-ident (raw "first"))
					(e-ident (raw "second")))
				(s-decl
					(p-ident (raw "second"))
					(e-int (raw "42")))))))
~~~
# FORMATTED
~~~roc
Fwd := [].{
	first = second
	second = 42
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Fwd.first"))
		(e-lookup-local
			(p-assign (ident "Fwd.second"))))
	(d-let
		(p-assign (ident "Fwd.second"))
		(e-num (value "42")))
	(s-nominal-decl
		(ty-header (name "Fwd"))
		(ty-tag-union)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Dec")))
	(type_decls
		(nominal (type "Fwd")
			(ty-header (name "Fwd"))))
	(expressions
		(expr (type "Dec"))
		(expr (type "Dec"))))
~~~
