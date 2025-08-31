# META
~~~ini
description=Simple identifier lookup canonicalization
type=expr
~~~
# SOURCE
~~~roc
foo
~~~
# TOKENS
~~~text
LowerIdent ~~~
# PARSE
~~~clojure
(lc "foo")
~~~
# FORMATTED
~~~roc
foo
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **foo** in this scope.
Is there an **import** or **exposing** missing up-top?

**expr_ident_simple.md:1:1:1:4:**
```roc
foo
```
^^^


# CANONICALIZE
~~~clojure
(Expr.lookup "foo")
~~~
# SOLVED
~~~clojure
(expr :tag lookup :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
