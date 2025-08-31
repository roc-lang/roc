# META
~~~ini
description=Debug expression stmt
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = Bool.True

expect foo != Bool.False
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpAssign UpperIdent Dot UpperIdent BlankLine KwExpect LowerIdent OpNotEquals UpperIdent Dot UpperIdent ~~~
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


foo = Bool.True
expect foo != Bool.False
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **Bool.True** in this scope.
Is there an **import** or **exposing** missing up-top?

**expect_stmt_top_level.md:3:7:3:16:**
```roc
foo = Bool.True
```
      ^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**expect_stmt_top_level.md:5:1:5:25:**
```roc
expect foo != Bool.False
```
^^^^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
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
