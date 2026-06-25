# META
~~~ini
description=A block-local alias of a polymorphic function inside a lambda body should stay polymorphic
type=snippet
~~~
# SOURCE
~~~roc
id = |x| x

main = |_y| {
    alias = id
    (alias(1), alias("a"))
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
OpenRound,LowerIdent,NoSpaceOpenRound,Int,CloseRound,Comma,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
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
			(p-ident (raw "main"))
			(e-lambda
				(args
					(p-ident (raw "_y")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "alias"))
							(e-ident (raw "id")))
						(e-tuple
							(e-apply
								(e-ident (raw "alias"))
								(e-int (raw "1")))
							(e-apply
								(e-ident (raw "alias"))
								(e-string
									(e-string-part (raw "a")))))))))))
~~~
# FORMATTED
~~~roc
id = |x| x

main = |_y| {
	alias = id
	(alias(1), alias("a"))
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
		(p-assign (ident "main"))
		(e-lambda
			(args
				(p-assign (ident "_y")))
			(e-block
				(s-let
					(p-assign (ident "alias"))
					(e-lookup-local
						(p-assign (ident "id"))))
				(e-tuple
					(elems
						(e-call (constraint-fn-var 64)
							(e-lookup-local
								(p-assign (ident "alias")))
							(e-num (value "1")))
						(e-call (constraint-fn-var 83)
							(e-lookup-local
								(p-assign (ident "alias")))
							(e-string
								(e-literal (string "a"))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "_arg -> (a, b) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)])]")))
	(expressions
		(expr (type "a -> a"))
		(expr (type "_arg -> (a, b) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)])]"))))
~~~
