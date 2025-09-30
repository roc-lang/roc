# META
~~~ini
description=Debug statement in body context
type=file
~~~
# SOURCE
~~~roc
main = {
    x = 42
    dbg x
    x + 1
}
~~~
# EXPECTED
MISSING MAIN! FUNCTION - dbg_stmt_in_body.md:1:1:5:2
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**dbg_stmt_in_body.md:1:1:5:2:**
```roc
main = {
    x = 42
    dbg x
    x + 1
}
```


# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpAssign(1:6-1:7),OpenCurly(1:8-1:9),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),Int(2:9-2:11),
KwDbg(3:5-3:8),LowerIdent(3:9-3:10),
LowerIdent(4:5-4:6),OpPlus(4:7-4:8),Int(4:9-4:10),
CloseCurly(5:1-5:2),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.2
	(type-module @1.1-1.5)
	(statements
		(s-decl @1.1-5.2
			(p-ident @1.1-1.5 (raw "main"))
			(e-block @1.8-5.2
				(statements
					(s-decl @2.5-2.11
						(p-ident @2.5-2.6 (raw "x"))
						(e-int @2.9-2.11 (raw "42")))
					(s-dbg @3.5-3.10
						(e-ident @3.9-3.10 (raw "x")))
					(e-binop @4.5-4.10 (op "+")
						(e-ident @4.5-4.6 (raw "x"))
						(e-int @4.9-4.10 (raw "1"))))))))
~~~
# FORMATTED
~~~roc
main = {
	x = 42
	dbg x
	x + 1
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.5 (ident "main"))
		(e-block @1.8-5.2
			(s-let @2.5-2.11
				(p-assign @2.5-2.6 (ident "x"))
				(e-int @2.9-2.11 (value "42")))
			(s-dbg @3.5-3.10
				(e-lookup-local @3.9-3.10
					(p-assign @2.5-2.6 (ident "x"))))
			(e-binop @4.5-4.10 (op "add")
				(e-lookup-local @4.5-4.6
					(p-assign @2.5-2.6 (ident "x")))
				(e-int @4.9-4.10 (value "1"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.5 (type "Num(_size)")))
	(expressions
		(expr @1.8-5.2 (type "Num(_size)"))))
~~~
