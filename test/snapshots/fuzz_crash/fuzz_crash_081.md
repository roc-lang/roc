# META
~~~ini
description=fuzz crash: 0.() incorrectly tokenized as float
type=snippet
~~~
# SOURCE
~~~roc
x = 0.()
~~~
# EXPECTED
NOT IMPLEMENTED - fuzz_crash_081.md:1:5:1:9
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: nominal value/tuple construction

**fuzz_crash_081.md:1:5:1:9:**
```roc
x = 0.()
```
    ^^^^

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!


# TOKENS
~~~zig
LowerIdent,OpAssign,Int,Dot,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-nominal-apply
				(mapper (e-int (raw "0")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "not_implemented"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
