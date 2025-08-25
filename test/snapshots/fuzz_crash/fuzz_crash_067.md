# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module []

f = || {
    crash 1
}
~~~
# EXPECTED
CRASH EXPECTS STRING - fuzz_crash_067.md:3:8:5:2
# PROBLEMS
**CRASH EXPECTS STRING**
The `crash` keyword expects a string literal as its argument.
For example: `crash "Something went wrong"`
**fuzz_crash_067.md:3:8:5:2:**
```roc
f = || {
    crash 1
}
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),OpBar(3:5-3:6),OpBar(3:6-3:7),OpenCurly(3:8-3:9),
KwCrash(4:5-4:10),Int(4:11-4:12),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(file @1.1-5.2
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @3.1-5.2
			(p-ident @3.1-3.2 (raw "f"))
			(e-lambda @3.5-5.2
				(args)
				(e-block @3.8-5.2
					(statements
						(s-crash @4.5-4.12
							(e-int @4.11-4.12 (raw "1")))))))))
~~~
# FORMATTED
~~~roc
module []

f = || {
	crash 1
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.2 (ident "f"))
		(e-lambda @3.5-5.2
			(args)
			(e-block @3.8-5.2
				(e-runtime-error (tag "crash_expects_string"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "({}) -> Error")))
	(expressions
		(expr @3.5-5.2 (type "({}) -> Error"))))
~~~
