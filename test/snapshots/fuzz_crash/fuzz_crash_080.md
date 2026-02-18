# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
c : L
        where [
                o
                .h : a,
        ]
~~~
# EXPECTED
UNDECLARED TYPE - fuzz_crash_080.md:1:5:1:6
# PROBLEMS
**UNDECLARED TYPE**
The type _L_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_080.md:1:5:1:6:**
```roc
c : L
```
    ^


# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
KwWhere,OpenSquare,
LowerIdent,
DotLowerIdent,OpColon,LowerIdent,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "c")
			(ty (name "L"))
			(where
				(method (module-of "o") (name "h")
					(args)
					(ty-var (raw "a")))))))
~~~
# FORMATTED
~~~roc
c : L
	where [
		o
		.h : a,
	]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "c"))
		(e-anno-only)
		(annotation
			(ty-malformed)
			(where
				(method (ty-rigid-var (name "o")) (name "h")
					(args)
					(ty-rigid-var (name "a")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
