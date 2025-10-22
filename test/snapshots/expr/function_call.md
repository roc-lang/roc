# META
~~~ini
description=Function call expression
type=expr
~~~
# SOURCE
~~~roc
add(5, 3)
~~~
# EXPECTED
UNDEFINED VARIABLE - function_call.md:1:1:1:4
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `add` in this scope.
Is there an `import` or `exposing` missing up-top?

**function_call.md:1:1:1:4:**
```roc
add(5, 3)
```
^^^


# TOKENS
~~~zig
LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-ident (raw "add"))
	(e-int (raw "5"))
	(e-int (raw "3")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-num (value "5"))
	(e-num (value "3")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
