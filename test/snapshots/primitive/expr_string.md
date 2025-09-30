# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
name = "luc"
foo = "hello ${name}"
~~~
# EXPECTED
MISSING MAIN! FUNCTION - expr_string.md:1:1:2:22
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**expr_string.md:1:1:2:22:**
```roc
name = "luc"
foo = "hello ${name}"
```


# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpAssign(1:6-1:7),StringStart(1:8-1:9),StringPart(1:9-1:12),StringEnd(1:12-1:13),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),StringStart(2:7-2:8),StringPart(2:8-2:14),OpenStringInterpolation(2:14-2:16),LowerIdent(2:16-2:20),CloseStringInterpolation(2:20-2:21),StringPart(2:21-2:21),StringEnd(2:21-2:22),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.22
	(type-module @1.1-1.5)
	(statements
		(s-decl @1.1-1.13
			(p-ident @1.1-1.5 (raw "name"))
			(e-string @1.8-1.13
				(e-string-part @1.9-1.12 (raw "luc"))))
		(s-decl @2.1-2.22
			(p-ident @2.1-2.4 (raw "foo"))
			(e-string @2.7-2.22
				(e-string-part @2.8-2.14 (raw "hello "))
				(e-ident @2.16-2.20 (raw "name"))
				(e-string-part @2.21-2.21 (raw ""))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.5 (ident "name"))
		(e-string @1.8-1.13
			(e-literal @1.9-1.12 (string "luc"))))
	(d-let
		(p-assign @2.1-2.4 (ident "foo"))
		(e-string @2.7-2.22
			(e-literal @2.8-2.14 (string "hello "))
			(e-lookup-local @2.16-2.20
				(p-assign @1.1-1.5 (ident "name")))
			(e-literal @2.21-2.21 (string "")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.5 (type "Str"))
		(patt @2.1-2.4 (type "Str")))
	(expressions
		(expr @1.8-1.13 (type "Str"))
		(expr @2.7-2.22 (type "Str"))))
~~~
