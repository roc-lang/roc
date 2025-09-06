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
UNDEFINED VARIABLE - expr_ident_simple.md:1:1:1:4
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
; Total type variables: 2
(var #0 _)
(var #1 _)
~~~
# TYPES
~~~roc
~~~
