# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]s:b->c where module(a).t:c,u:o...
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent TripleDot ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

s : (b -> c where module(a).t : c, u) : o
...
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_057.md:1:28:1:31:**
```roc
module[]s:b->c where module(a).t:c,u:o...
```
                           ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "s")
    (Expr.binop_colon
      (Expr.tuple_literal
        (Expr.binop_colon
          (Expr.binop_thin_arrow
            (Expr.lookup "b")
            (Expr.lookup "c")
          )
          (Expr.binop_colon
            (Expr.lambda)
            (Expr.lookup "c")
          )
        )
        (Expr.lookup "u")
      )
      (Expr.lookup "o")
    )
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
~~~
