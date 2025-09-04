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
OpenCurly(1:1-1:2),
LowerIdent(2:2-2:7),OpAssign(2:8-2:9),StringStart(2:10-2:11),StringPart(2:11-2:16),StringEnd(2:16-2:17),
LowerIdent(3:2-3:7),OpAssign(3:8-3:9),StringStart(3:10-3:11),StringPart(3:11-3:16),StringEnd(3:16-3:17),
StringStart(4:2-4:3),StringPart(4:3-4:3),OpenStringInterpolation(4:3-4:5),LowerIdent(4:5-4:10),CloseStringInterpolation(4:10-4:11),StringPart(4:11-4:12),OpenStringInterpolation(4:12-4:14),LowerIdent(4:14-4:19),CloseStringInterpolation(4:19-4:20),StringPart(4:20-4:20),StringEnd(4:20-4:21),
CloseCurly(5:1-5:2),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(e-block @1.1-5.2
	(statements
		(s-decl @2.2-2.17
			(p-ident @2.2-2.7 (raw "hello"))
			(e-string @2.10-2.17
				(e-string-part @2.11-2.16 (raw "Hello"))))
		(s-decl @3.2-3.17
			(p-ident @3.2-3.7 (raw "world"))
			(e-string @3.10-3.17
				(e-string-part @3.11-3.16 (raw "World"))))
		(e-string @4.2-4.21
			(e-string-part @4.3-4.3 (raw ""))
			(e-ident @4.5-4.10 (raw "hello"))
			(e-string-part @4.11-4.12 (raw " "))
			(e-ident @4.14-4.19 (raw "world"))
			(e-string-part @4.20-4.20 (raw "")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-5.2
	(s-let @2.2-2.17
		(p-assign @2.2-2.7 (ident "hello"))
		(e-string @2.10-2.17
			(e-literal @2.11-2.16 (string "Hello"))))
	(s-let @3.2-3.17
		(p-assign @3.2-3.7 (ident "world"))
		(e-string @3.10-3.17
			(e-literal @3.11-3.16 (string "World"))))
	(e-string @4.2-4.21
		(e-literal @4.3-4.3 (string ""))
		(e-lookup-local @4.5-4.10
			(p-assign @2.2-2.7 (ident "hello")))
		(e-literal @4.11-4.12 (string " "))
		(e-lookup-local @4.14-4.19
			(p-assign @3.2-3.7 (ident "world")))
		(e-literal @4.20-4.20 (string ""))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Str"))
~~~
