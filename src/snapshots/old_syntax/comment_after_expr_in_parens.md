# META
~~~ini
description=comment_after_expr_in_parens
type=expr
~~~
# SOURCE
~~~roc
(i#abc
)
~~~
# EXPECTED
UNDEFINED VARIABLE - comment_after_expr_in_parens.md:1:2:1:3
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

**comment_after_expr_in_parens.md:1:2:1:3:**
```roc
(i#abc
```
 ^


# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:3),Newline(1:4-1:7),
CloseRound(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-2.2
	(e-ident @1.2-1.3 (raw "i")))
~~~
# FORMATTED
~~~roc
(
	i, # abc
)
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.2-1.3 (type "Error"))
~~~
