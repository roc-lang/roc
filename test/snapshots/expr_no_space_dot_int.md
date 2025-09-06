# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
module []

foo = asd.0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign LowerIdent Dot Int ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

foo = (asd | 0)
~~~
# EXPECTED
PARSE ERROR - expr_no_space_dot_int.md:3:10:3:12
UNRECOGNIZED SYNTAX - expr_no_space_dot_int.md:3:10:3:12
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**expr_no_space_dot_int.md:3:7:3:12:**
```roc
foo = asd.0
```
      ^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
