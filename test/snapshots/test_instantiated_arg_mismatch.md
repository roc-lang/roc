# META
~~~ini
description=Type mismatch with instantiated function arguments
type=expr
~~~
# SOURCE
~~~roc
{
    pair : a, a -> (a, a)
    pair = |x, y| (x, y)

    pair(42, "hello")
}
~~~
# EXPECTED
TYPE MISMATCH - test_instantiated_arg_mismatch.md:5:14:5:21
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This string literal is being used where a non-string ──────┐
└┬──────────────┘  type is needed.                                            │
 │                                                                            │
 │  pair(42, "hello")                                                         │
 │           ‾‾‾‾‾‾‾                                                          │
 └──────────────────────────────────── test_instantiated_arg_mismatch.md:5:14 ┘

    The type was determined to be:

        Dec

# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-anno (name "pair")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "a"))
				(ty-tuple
					(ty-var (raw "a"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "pair"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-tuple
					(e-ident (raw "x"))
					(e-ident (raw "y")))))
		(e-apply
			(e-ident (raw "pair"))
			(e-int (raw "42"))
			(e-string
				(e-string-part (raw "hello"))))))
~~~
# FORMATTED
~~~roc
{
	pair : a, a -> (a, a)
	pair = |x, y| (x, y)

	pair(42, "hello")
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "pair"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-tuple
				(elems
					(e-lookup-local
						(p-assign (ident "x")))
					(e-lookup-local
						(p-assign (ident "y")))))))
	(e-call (constraint-fn-var 83)
		(e-lookup-local
			(p-assign (ident "pair")))
		(e-num (value "42"))
		(e-string
			(e-literal (string "hello")))))
~~~
# TYPES
~~~clojure
(expr (type "(Dec, Dec)"))
~~~
