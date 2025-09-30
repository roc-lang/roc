# META
~~~ini
description=Multiline without comma formatting module
type=file
~~~
# SOURCE
~~~roc
a = 'a'
b = 'a'
~~~
# EXPECTED
MISSING MAIN! FUNCTION - module.md:1:1:2:8
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**module.md:1:1:2:8:**
```roc
a = 'a'
b = 'a'
```


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),SingleQuote(1:5-1:8),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),SingleQuote(2:5-2:8),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.8
	(type-module @1.1-1.2)
	(statements
		(s-decl @1.1-1.8
			(p-ident @1.1-1.2 (raw "a"))
			(e-single-quote @1.5-1.8 (raw "'a'")))
		(s-decl @2.1-2.8
			(p-ident @2.1-2.2 (raw "b"))
			(e-single-quote @2.5-2.8 (raw "'a'")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.2 (ident "a"))
		(e-int @1.5-1.8 (value "97")))
	(d-let
		(p-assign @2.1-2.2 (ident "b"))
		(e-int @2.5-2.8 (value "97"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.2 (type "Num(_size)"))
		(patt @2.1-2.2 (type "Num(_size)")))
	(expressions
		(expr @1.5-1.8 (type "Num(_size)"))
		(expr @2.5-2.8 (type "Num(_size)"))))
~~~
