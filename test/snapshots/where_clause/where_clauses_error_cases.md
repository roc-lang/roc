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
KwModule OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpArrow LowerIdent LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "broken_fn1")
    (binop_thin_arrow
      (binop_where
        (binop_thin_arrow
          (lc "a")
          (lc "b")
        )
        (binop_pipe
          (apply_module
            (lc "a")
          )
          (dot_lc "method")
        )
      )
      (lc "b")
    )
  )
  (binop_colon
    (lc "broken_fn2")
    (binop_thin_arrow
      (binop_where
        (binop_thin_arrow
          (binop_where
            (binop_thin_arrow
              (lc "a")
              (lc "b")
            )
            (binop_colon
              (lc "broken_fn3")
              (lc "a")
            )
          )
          (lc "b")
        )
        (binop_colon
          (binop_pipe
            (apply_module
              (lc "c")
            )
            (dot_lc "method")
          )
          (lc "c")
        )
      )
      (lc "d")
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
WHERE CLAUSE ERROR - where_clauses_error_cases.md:6:5:6:11
PARSE ERROR - where_clauses_error_cases.md:6:25:6:26
WHERE CLAUSE ERROR - where_clauses_error_cases.md:10:3:10:8
MALFORMED WHERE CLAUSE - where_clauses_error_cases.md:6:5:6:24
MALFORMED WHERE CLAUSE - where_clauses_error_cases.md:10:3:10:8
EXPOSED BUT NOT DEFINED - where_clauses_error_cases.md:1:9:1:19
EXPOSED BUT NOT DEFINED - where_clauses_error_cases.md:1:21:1:31
EXPOSED BUT NOT DEFINED - where_clauses_error_cases.md:1:33:1:43
# PROBLEMS
**Unsupported Node**
at 4:14 to 6:26

**Unsupported Node**
at 9:14 to 15:30

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "broken_fn1")
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "broken_fn2")
    (Expr.malformed)
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
