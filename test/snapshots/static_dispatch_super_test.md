# META
~~~ini
description=Dot access super test
type=expr
~~~
# SOURCE
~~~roc
some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
~~~
# TOKENS
~~~text
LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
?
~~~
# EXPECTED
NOT IMPLEMENTED - :0:0:0:0
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**static_dispatch_super_test.md:1:14:1:15:**
```roc
some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
```
             ^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
