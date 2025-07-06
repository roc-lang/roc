# META
~~~ini
description=try_subtract
type=expr
~~~
# SOURCE
~~~roc
try-w
~~~
# EXPECTED
UNDEFINED VARIABLE - try_subtract.md:1:1:1:4
UNDEFINED VARIABLE - try_subtract.md:1:5:1:6
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `try` in this scope.
Is there an `import` or `exposing` missing up-top?

**try_subtract.md:1:1:1:4:**
```roc
try-w
```
^^^


**UNDEFINED VARIABLE**
Nothing is named `w` in this scope.
Is there an `import` or `exposing` missing up-top?

**try_subtract.md:1:5:1:6:**
```roc
try-w
```
    ^


# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpBinaryMinus(1:4-1:5),LowerIdent(1:5-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.6 (op "-")
	(e-ident @1.1-1.4 (raw "try"))
	(e-ident @1.5-1.6 (raw "w")))
~~~
# FORMATTED
~~~roc
try - w
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.6 (op "sub")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "*"))
~~~
