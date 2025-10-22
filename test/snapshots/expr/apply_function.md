# META
~~~ini
description=Function application expression
type=expr
~~~
# SOURCE
~~~roc
foo(42, "hello")
~~~
# EXPECTED
UNDEFINED VARIABLE - apply_function.md:1:1:1:4
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**apply_function.md:1:1:1:4:**
```roc
foo(42, "hello")
```
^^^


# TOKENS
~~~zig
LowerIdent,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-ident (raw "foo"))
	(e-int (raw "42"))
	(e-string
		(e-string-part (raw "hello"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-num (value "42"))
	(e-string
		(e-literal (string "hello"))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
