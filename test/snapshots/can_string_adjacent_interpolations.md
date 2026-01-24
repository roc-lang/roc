# META
~~~ini
description=Test string with adjacent interpolations
type=expr
~~~
# SOURCE
~~~roc
{
    a = "hello"
    b = "world"
    "${a}${b}!"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-string
				(e-string-part (raw "hello"))))
		(s-decl
			(p-ident (raw "b"))
			(e-string
				(e-string-part (raw "world"))))
		(e-string
			(e-string-part (raw ""))
			(e-ident (raw "a"))
			(e-string-part (raw ""))
			(e-ident (raw "b"))
			(e-string-part (raw "!")))))
~~~
# FORMATTED
~~~roc
{
	a = "hello"
	b = "world"
	"${a}${b}!"
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "a"))
		(e-string
			(e-literal (string "hello"))))
	(s-let
		(p-assign (ident "b"))
		(e-string
			(e-literal (string "world"))))
	(e-string
		(e-literal (string ""))
		(e-lookup-local
			(p-assign (ident "a")))
		(e-literal (string ""))
		(e-lookup-local
			(p-assign (ident "b")))
		(e-literal (string "!"))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
