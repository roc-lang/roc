# META
~~~ini
description=Test list spread pattern
type=expr
~~~
# SOURCE
~~~roc
|[first, .. as rest]| first
~~~
# EXPECTED
UNUSED VARIABLE - can_spread_pattern.md:1:1:1:1
# PROBLEMS
**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**can_spread_pattern.md:1:1:1:1:**
```roc
|[first, .. as rest]| first
```
^


# TOKENS
~~~zig
OpBar,OpenSquare,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpBar,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-list
			(p-ident (raw "first"))
			(p-list-rest (name "rest"))))
	(e-ident (raw "first")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-list
			(patterns
				(p-assign (ident "first")))
			(rest-at (index 1)
				(p-assign (ident "rest")))))
	(e-lookup-local
		(p-assign (ident "first"))))
~~~
# TYPES
~~~clojure
(expr (type "List(a) -> a"))
~~~
