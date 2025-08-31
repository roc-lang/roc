# META
~~~ini
description=Simple test for single underscore type becoming error type
type=file
~~~
# SOURCE
~~~roc
module []

BadType := _

foo : BadType
foo = 42
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpColonEqual Underscore BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []


BadType := _
foo : BadType
foo = 42
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**simple_underscore_error.md:3:9:3:11:**
```roc
BadType := _
```
        ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
