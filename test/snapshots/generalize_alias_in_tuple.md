# META
~~~ini
description=A tuple holding references to a polymorphic function, destructured; one element is then used at two types
type=snippet
~~~
# SOURCE
~~~roc
id = |x| x

t = (id, id)

main = {
    (a, b) = t
    (a(1), a("x"), b(2))
}
~~~
# EXPECTED
TYPE MISMATCH - generalize_alias_in_tuple.md:7:14:7:17
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This string literal is being used where a non-string ──────┐
└┬──────────────┘  type is needed.                                            │
 │                                                                            │
 │  (a(1), a("x"), b(2))                                                      │
 │           ‾‾‾                                                              │
 └───────────────────────────────────────── generalize_alias_in_tuple.md:7:14 ┘

    The type was determined to be:

        Dec

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpenCurly,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,LowerIdent,
OpenRound,LowerIdent,NoSpaceOpenRound,Int,CloseRound,Comma,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,Comma,LowerIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "id"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-decl
			(p-ident (raw "t"))
			(e-tuple
				(e-ident (raw "id"))
				(e-ident (raw "id"))))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-tuple
							(p-ident (raw "a"))
							(p-ident (raw "b")))
						(e-ident (raw "t")))
					(e-tuple
						(e-apply
							(e-ident (raw "a"))
							(e-int (raw "1")))
						(e-apply
							(e-ident (raw "a"))
							(e-string
								(e-string-part (raw "x"))))
						(e-apply
							(e-ident (raw "b"))
							(e-int (raw "2")))))))))
~~~
# FORMATTED
~~~roc
id = |x| x

t = (id, id)

main = {
	(a, b) = t
	(a(1), a("x"), b(2))
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "id"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(d-let
		(p-assign (ident "t"))
		(e-tuple
			(elems
				(e-lookup-local
					(p-assign (ident "id")))
				(e-lookup-local
					(p-assign (ident "id"))))))
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-tuple
					(patterns
						(p-assign (ident "a"))
						(p-assign (ident "b"))))
				(e-lookup-local
					(p-assign (ident "t"))))
			(e-tuple
				(elems
					(e-call (constraint-fn-var 72)
						(e-lookup-local
							(p-assign (ident "a")))
						(e-num (value "1")))
					(e-call (constraint-fn-var 89)
						(e-lookup-local
							(p-assign (ident "a")))
						(e-string
							(e-literal (string "x"))))
					(e-call (constraint-fn-var 123)
						(e-lookup-local
							(p-assign (ident "b")))
						(e-num (value "2"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c -> c"))
		(patt (type "(Dec -> Dec, Dec -> Dec)"))
		(patt (type "(Dec, Dec, Dec)")))
	(expressions
		(expr (type "c -> c"))
		(expr (type "(Dec -> Dec, Dec -> Dec)"))
		(expr (type "(Dec, Dec, Dec)"))))
~~~
