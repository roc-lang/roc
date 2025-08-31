# META
~~~ini
description=unary_not
type=expr
~~~
# SOURCE
~~~roc
!blah
~~~
# TOKENS
~~~text
OpBang LowerIdent ~~~
# PARSE
~~~clojure
(unary_not <unary>)
~~~
# FORMATTED
~~~roc
!blah
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **blah** in this scope.
Is there an **import** or **exposing** missing up-top?

**unary_not.md:1:2:1:6:**
```roc
!blah
```
 ^^^^


# CANONICALIZE
~~~clojure
(Expr.unary_not)
~~~
# SOLVED
~~~clojure
(expr :tag unary_not :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~
