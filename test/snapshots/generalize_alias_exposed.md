# META
~~~ini
description=A module that exposes a binding aliasing a polymorphic function
type=file
~~~
# SOURCE
~~~roc
FooBar := {}.{
    myfunc : List(a) -> U64
    myfunc = |list| list.len()
}

shorthand = FooBar.myfunc
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "FooBar")
				(args))
			(ty-record)
			(associated
				(s-type-anno (name "myfunc")
					(ty-fn
						(ty-apply
							(ty (name "List"))
							(ty-var (raw "a")))
						(ty (name "U64"))))
				(s-decl
					(p-ident (raw "myfunc"))
					(e-lambda
						(args
							(p-ident (raw "list")))
						(e-method-call (method ".len")
							(receiver
								(e-ident (raw "list")))
							(args))))))
		(s-decl
			(p-ident (raw "shorthand"))
			(e-ident (raw "FooBar.myfunc")))))
~~~
# FORMATTED
~~~roc
FooBar := {}.{
	myfunc : List(a) -> U64
	myfunc = |list| list.len()
}

shorthand = FooBar.myfunc
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "generalize_alias_exposed.FooBar.myfunc"))
		(e-lambda
			(args
				(p-assign (ident "list")))
			(e-dispatch-call (method "len") (constraint-fn-var 39)
				(receiver
					(e-lookup-local
						(p-assign (ident "list"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "a")))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "shorthand"))
		(e-lookup-local
			(p-assign (ident "generalize_alias_exposed.FooBar.myfunc"))))
	(s-nominal-decl
		(ty-header (name "FooBar"))
		(ty-record)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(a) -> U64"))
		(patt (type "List(a) -> U64")))
	(type_decls
		(nominal (type "FooBar")
			(ty-header (name "FooBar"))))
	(expressions
		(expr (type "List(a) -> U64"))
		(expr (type "List(a) -> U64"))))
~~~
