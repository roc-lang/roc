# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
f = || {
    crash 1
}
~~~
# EXPECTED
MISSING MAIN! FUNCTION - fuzz_crash_067.md:1:1:3:2
CRASH EXPECTS STRING - fuzz_crash_067.md:1:8:3:2
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_067.md:1:1:3:2:**
```roc
f = || {
    crash 1
}
```


**CRASH EXPECTS STRING**
The `crash` keyword expects a string literal as its argument.
For example: `crash "Something went wrong"`
**fuzz_crash_067.md:1:8:3:2:**
```roc
f = || {
    crash 1
}
```


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),OpBar(1:5-1:6),OpBar(1:6-1:7),OpenCurly(1:8-1:9),
KwCrash(2:5-2:10),Int(2:11-2:12),
CloseCurly(3:1-3:2),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.2
	(type-module @1.1-1.2)
	(statements
		(s-decl @1.1-3.2
			(p-ident @1.1-1.2 (raw "f"))
			(e-lambda @1.5-3.2
				(args)
				(e-block @1.8-3.2
					(statements
						(s-crash @2.5-2.12
							(e-int @2.11-2.12 (raw "1")))))))))
~~~
# FORMATTED
~~~roc
f = || {
	crash 1
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.2 (ident "f"))
		(e-lambda @1.5-3.2
			(args)
			(e-block @1.8-3.2
				(e-runtime-error (tag "crash_expects_string"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.2 (type "({}) -> Error")))
	(expressions
		(expr @1.5-3.2 (type "({}) -> Error"))))
~~~
