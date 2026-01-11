# META
~~~ini
description=Minus without spaces in function call
type=expr
~~~
# SOURCE
~~~roc
foo(x-1)
~~~
# EXPECTED
UNDEFINED VARIABLE - minus_no_space_in_call.md:1:1:1:4
UNDEFINED VARIABLE - minus_no_space_in_call.md:1:5:1:6
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**minus_no_space_in_call.md:1:1:1:4:**
```roc
foo(x-1)
```
^^^


**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**minus_no_space_in_call.md:1:5:1:6:**
```roc
foo(x-1)
```
    ^


# TOKENS
~~~zig
LowerIdent,NoSpaceOpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-ident (raw "foo"))
	(e-binop (op "-")
		(e-ident (raw "x"))
		(e-int (raw "1"))))
~~~
# FORMATTED
~~~roc
foo(x - 1)
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-binop (op "sub")
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-num (value "1"))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
