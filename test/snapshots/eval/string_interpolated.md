# META
~~~ini
description=Tuple containing variations on boolean values
type=expr
~~~
# SOURCE
~~~roc
{
	hello = "Hello"
	world = "World"
	"${hello} ${world}"
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
			(p-ident (raw "hello"))
			(e-string
				(e-string-part (raw "Hello"))))
		(s-decl
			(p-ident (raw "world"))
			(e-string
				(e-string-part (raw "World"))))
		(e-string
			(e-string-part (raw ""))
			(e-ident (raw "hello"))
			(e-string-part (raw " "))
			(e-ident (raw "world"))
			(e-string-part (raw "")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "hello"))
		(e-string
			(e-literal (string "Hello"))))
	(s-let
		(p-assign (ident "world"))
		(e-string
			(e-literal (string "World"))))
	(e-string
		(e-literal (string ""))
		(e-lookup-local
			(p-assign (ident "hello")))
		(e-literal (string " "))
		(e-lookup-local
			(p-assign (ident "world")))
		(e-literal (string ""))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
