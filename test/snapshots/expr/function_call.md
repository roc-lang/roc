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
LowerIdent(1:1-1:4),NoSpaceOpenRound(1:4-1:5),Int(1:5-1:6),Comma(1:6-1:7),Int(1:8-1:9),CloseRound(1:9-1:10),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.10
	(e-ident @1.1-1.4 (raw "add"))
	(e-int @1.5-1.6 (raw "5"))
	(e-int @1.8-1.9 (raw "3")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.10
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-int @1.5-1.6 (value "5"))
	(e-int @1.8-1.9 (value "3")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.10 (type "_a"))
~~~
