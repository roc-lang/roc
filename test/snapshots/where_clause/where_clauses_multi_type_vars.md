# META
~~~ini
description=Multiple where constraints on different type variables
type=file
~~~
# SOURCE
~~~roc
module [process]

process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
process = |_, _| ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar Underscore Comma Underscore OpBar TripleDot ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "process")
))
~~~
# FORMATTED
~~~roc
module [process]

process : a -> b -> (c where module(a).convert : a) -> c, module(b).transform : b -> c
process = |_, _| ...
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_multi_type_vars.md:3:33:3:36:**
```roc
process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
```
                                ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_multi_type_vars.md:3:61:3:64:**
```roc
process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
```
                                                            ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "process")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.binop_thin_arrow
        (Expr.lookup "b")
        (Expr.binop_thin_arrow
          (Expr.binop_colon
            (Expr.tuple_literal
              (Expr.binop_thin_arrow
                (Expr.binop_colon
                  (Expr.lookup "c")
                  (Expr.binop_colon
                    (Expr.lambda)
                    (Expr.lookup "a")
                  )
                )
                (Expr.lookup "c")
              )
              (Expr.lambda)
            )
            (Expr.lookup "b")
          )
          (Expr.lookup "c")
        )
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "process")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
process : _d
~~~
