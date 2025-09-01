# META
~~~ini
description=unary_negation_access
type=expr
~~~
# SOURCE
~~~roc
-rec1.field
~~~
# TOKENS
~~~text
OpUnaryMinus LowerIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(unary_neg <unary_op>)
~~~
# FORMATTED
~~~roc
-rec1.field
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**unary_negation_access.md:1:2:1:12:**
```roc
-rec1.field
```
 ^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.unary_neg)
~~~
# SOLVED
~~~clojure
(expr :tag unary_neg :type "Num(_size)")
~~~
# TYPES
~~~roc
Num(_size)
~~~
