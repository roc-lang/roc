# META
~~~ini
description=fuzz crash, unterminated single quote
type=file
~~~
# SOURCE
~~~roc
mule []

#el
vavar t= '
~~~
# TOKENS
~~~text
LowerIdent OpenSquare CloseSquare BlankLine LineComment LowerIdent LowerIdent OpAssign MalformedSingleQuoteUnclosed ~~~
# PARSE
~~~clojure
(block
  (lc "mule")
  (list_literal)
  (lc "vavar")
  (binop_equals
    (lc "t")
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
mule
[]
#el
vavar
t = '
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **'** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_031.md:4:10:4:11:**
```roc
vavar t= '
```
         ^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_031.md:1:1:1:5:**
```roc
mule []
```
^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_031.md:1:6:1:8:**
```roc
mule []
```
     ^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_031.md:4:1:4:6:**
```roc
vavar t= '
```
^^^^^


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
