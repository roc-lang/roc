# META
~~~ini
description=Simple addition function with expect statement
type=file
~~~
# SOURCE
~~~roc
module [addU8]

addU8 : U8, U8 -> U8
addU8 = |a, b| a + b

expect addU8(1, 2) == 3
expect addU8(0, 10) == 10
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpPlus LowerIdent BlankLine KwExpect LowerIdent OpenRound Int Comma Int CloseRound OpEquals Int KwExpect LowerIdent OpenRound Int Comma Int CloseRound OpEquals Int ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "addU8")
))
~~~
# FORMATTED
~~~roc
module [addU8]


addU8 : U8 -> U8 -> U8
addU8 = |a, b| a + b
expect addU8((1, 2)) == 3
expect addU8((0, 10)) == 10
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**simple_add.md:6:1:6:24:**
```roc
expect addU8(1, 2) == 3
```
^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**simple_add.md:7:1:7:26:**
```roc
expect addU8(0, 10) == 10
```
^^^^^^^^^^^^^^^^^^^^^^^^^


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
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
