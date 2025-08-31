# META
~~~ini
description=Debug expression not permitted at the top level
type=file
~~~
# SOURCE
~~~roc
module [foo]

# not permitted
dbg "foo"

foo = ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LineComment KwDbg String BlankLine LowerIdent OpAssign TripleDot ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")
))
~~~
# FORMATTED
~~~roc
module [foo]


# not permitted
dbg 
"foo"
foo = ...
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**dbg_stmt_not_permitted_top_level.md:4:1:4:5:**
```roc
dbg "foo"
```
^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**dbg_stmt_not_permitted_top_level.md:4:1:4:5:**
```roc
dbg "foo"
```
^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**dbg_stmt_not_permitted_top_level.md:4:5:4:10:**
```roc
dbg "foo"
```
    ^^^^^


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
