# META
~~~ini
description=Minimal test - underscore type should become error type
type=file
~~~
# SOURCE
~~~roc
module []

BadType := _
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpColonEqual Underscore ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []


BadType := _
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**minimal_underscore.md:3:9:3:11:**
```roc
BadType := _
```
        ^^


# CANONICALIZE
~~~clojure
(Expr.block
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
