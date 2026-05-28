# META
~~~ini
description=Simple debug test to understand parsing behavior
type=snippet
~~~
# SOURCE
~~~roc
test = {
    x = 42
    dbg(x)
}
~~~
# EXPECTED
EFFECTFUL TOP-LEVEL VALUE - dbg_simple_test.md:1:8:4:2
# PROBLEMS
**EFFECTFUL TOP-LEVEL VALUE**
This top-level definition performs an effect while initializing.
**dbg_simple_test.md:1:8:4:2:**
```roc
test = {
    x = 42
    dbg(x)
}
```


Move the effect into a function body so it runs when the function is called.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,Int,
KwDbg,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "test"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "x"))
						(e-int (raw "42")))
					(s-dbg
						(e-tuple
							(e-ident (raw "x")))))))))
~~~
# FORMATTED
~~~roc
test = {
	x = 42
	dbg (x)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "test"))
		(e-block
			(s-let
				(p-assign (ident "x"))
				(e-num (value "42")))
			(e-dbg
				(e-lookup-local
					(p-assign (ident "x")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
