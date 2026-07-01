# META
~~~ini
description=issue 9574 - a binding whose RHS is a bare reference to a polymorphic function is non-expansive, so it stays polymorphic (value restriction)
type=file
~~~
# SOURCE
~~~roc
FooBar := {}.{
    myfunc : List(a) -> U64
    myfunc = |list| list.len()
}

shorthand = FooBar.myfunc

main = {
    int_list = shorthand([1, 2, 3])
    string_list = shorthand(["a", "b", "c"])
    (int_list, string_list)
}
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
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,CloseRound,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
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
			(e-ident (raw "FooBar.myfunc")))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "int_list"))
						(e-apply
							(e-ident (raw "shorthand"))
							(e-list
								(e-int (raw "1"))
								(e-int (raw "2"))
								(e-int (raw "3")))))
					(s-decl
						(p-ident (raw "string_list"))
						(e-apply
							(e-ident (raw "shorthand"))
							(e-list
								(e-string
									(e-string-part (raw "a")))
								(e-string
									(e-string-part (raw "b")))
								(e-string
									(e-string-part (raw "c"))))))
					(e-tuple
						(e-ident (raw "int_list"))
						(e-ident (raw "string_list"))))))))
~~~
# FORMATTED
~~~roc
FooBar := {}.{
	myfunc : List(a) -> U64
	myfunc = |list| list.len()
}

shorthand = FooBar.myfunc

main = {
	int_list = shorthand([1, 2, 3])
	string_list = shorthand(["a", "b", "c"])
	(int_list, string_list)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "shorthand_polymorphic_method.FooBar.myfunc"))
		(e-lambda
			(args
				(p-assign (ident "list")))
			(e-dispatch-call (method "len") (constraint-fn-var 64)
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
			(p-assign (ident "shorthand_polymorphic_method.FooBar.myfunc"))))
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "int_list"))
				(e-call (constraint-fn-var 189)
					(e-lookup-local
						(p-assign (ident "shorthand")))
					(e-list
						(elems
							(e-num (value "1"))
							(e-num (value "2"))
							(e-num (value "3"))))))
			(s-let
				(p-assign (ident "string_list"))
				(e-call (constraint-fn-var 243)
					(e-lookup-local
						(p-assign (ident "shorthand")))
					(e-list
						(elems
							(e-string
								(e-literal (string "a")))
							(e-string
								(e-literal (string "b")))
							(e-string
								(e-literal (string "c")))))))
			(e-tuple
				(elems
					(e-lookup-local
						(p-assign (ident "int_list")))
					(e-lookup-local
						(p-assign (ident "string_list")))))))
	(s-nominal-decl
		(ty-header (name "FooBar"))
		(ty-record)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(a) -> U64"))
		(patt (type "List(a) -> U64"))
		(patt (type "(U64, U64)")))
	(type_decls
		(nominal (type "FooBar")
			(ty-header (name "FooBar"))))
	(expressions
		(expr (type "List(a) -> U64"))
		(expr (type "List(a) -> U64"))
		(expr (type "(U64, U64)"))))
~~~
