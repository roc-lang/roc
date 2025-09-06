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
(block
  (binop_colon_equals
    (uc "BadType")
    (underscore)
  )
)
~~~
# FORMATTED
~~~roc
module []

BadType := _
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - minimal_underscore.md:1:1:1:1
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
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
~~~
# TYPES
~~~roc
~~~
