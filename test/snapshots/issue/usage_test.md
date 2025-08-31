# META
~~~ini
description=Test if usage affects error type conversion
type=file
~~~
# SOURCE
~~~roc
module []

UnusedType := _

UsedType := _

value : UsedType
value = 42
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpColonEqual Underscore BlankLine UpperIdent OpColonEqual Underscore BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []


UnusedType := _
UsedType := _
value : UsedType
value = 42
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**usage_test.md:3:12:3:14:**
```roc
UnusedType := _
```
           ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**usage_test.md:5:10:5:12:**
```roc
UsedType := _
```
         ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
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
