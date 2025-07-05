# META
~~~ini
description=binop_apply_complex
type=expr
~~~
# SOURCE
~~~roc
N<l (r*N)
~~~
# EXPECTED
UNDEFINED VARIABLE - binop_apply_complex.md:1:3:1:4
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `l` in this scope.
Is there an `import` or `exposing` missing up-top?

**binop_apply_complex.md:1:3:1:4:**
```roc
N<l (r*N)
```
  ^


# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpLessThan(1:2-1:3),LowerIdent(1:3-1:4),OpenRound(1:5-1:6),LowerIdent(1:6-1:7),OpStar(1:7-1:8),UpperIdent(1:8-1:9),CloseRound(1:9-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.6 (op "<")
	(e-tag @1.1-1.2 (raw "N"))
	(e-ident @1.3-1.4 (raw "l")))
~~~
# FORMATTED
~~~roc
N < l
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.6 (op "lt")
	(e-tag @1.1-1.2 (name "N"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "*"))
~~~
