# META
~~~ini
description=Dot access expression
type=expr
~~~
# SOURCE
~~~roc
list.map(fn)
~~~
# TOKENS
~~~text
LowerIdent Dot LowerIdent OpenRound LowerIdent CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (binop_pipe
    (lc "list")
    (dot_lc "map")
  )
  (lc "fn")
)
~~~
# FORMATTED
~~~roc
list.map(fn)
~~~
# EXPECTED
UNDEFINED VARIABLE - can_dot_access.md:1:1:1:5
UNDEFINED VARIABLE - can_dot_access.md:1:10:1:12
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **list** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_dot_access.md:1:1:1:5:**
```roc
list.map(fn)
```
^^^^


**UNDEFINED VARIABLE**
Nothing is named **fn** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_dot_access.md:1:10:1:12:**
```roc
list.map(fn)
```
         ^^


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 7
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 -> #6)
(var #4 _)
(var #5 _)
(var #6 fn_pure)
~~~
# TYPES
~~~roc
~~~
