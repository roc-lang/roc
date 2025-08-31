# META
~~~ini
description=Error cases for where clauses
type=file
~~~
# SOURCE
~~~roc
module [broken_fn1, broken_fn2, broken_fn3]

# Missing colon in constraint
broken_fn1 : a -> b
  where
    module(a).method -> b

# Empty where clause
broken_fn2 : a -> b
  where

# Referencing undefined type variable
broken_fn3 : a -> b
  where
    module(c).method : c -> d
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpArrow LowerIdent BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "broken_fn1")

    (lc "broken_fn2")

    (lc "broken_fn3")
))
~~~
# FORMATTED
~~~roc
module [broken_fn1, broken_fn2, broken_fn3]

broken_fn1 : a -> b where module(a).method -> b
broken_fn2 : a -> b where broken_fn3 : a -> b where module(c).method : c -> d# Missing colon in constraint
# Empty where clause
# Referencing undefined type variable
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_error_cases.md:6:11:6:14:**
```roc
    module(a).method -> b
```
          ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_error_cases.md:15:11:15:14:**
```roc
    module(c).method : c -> d
```
          ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "broken_fn1")
    (Expr.binop_thin_arrow
      (Expr.binop_colon
        (Expr.binop_thin_arrow
          (Expr.lookup "a")
          (Expr.lookup "b")
        )
        (Expr.lambda)
      )
      (Expr.lookup "b")
    )
  )
  (Expr.binop_colon
    (Expr.lookup "broken_fn2")
    (Expr.binop_thin_arrow
      (Expr.binop_colon
        (Expr.binop_thin_arrow
          (Expr.binop_colon
            (Expr.binop_thin_arrow
              (Expr.lookup "a")
              (Expr.lookup "b")
            )
            (Expr.binop_colon
              (Expr.lookup "broken_fn3")
              (Expr.lookup "a")
            )
          )
          (Expr.lookup "b")
        )
        (Expr.binop_colon
          (Expr.lambda)
          (Expr.lookup "c")
        )
      )
      (Expr.lookup "d")
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_e")
~~~
# TYPES
~~~roc
~~~
