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
(block
  (binop_colon
    (lc "broken_fn1")
    (binop_arrow_call
      (binop_where
        (binop_arrow_call
          (lc "a")
          (lc "b")
        )
        (binop_dot
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
    (binop_arrow_call
      (binop_where
        (binop_arrow_call
          (binop_where
            (binop_arrow_call
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
          (binop_dot
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
module [broken_fn1, broken_fn2, broken_fn3]

# Missing colon in constraint
broken_fn1 : a -> b where module(a)..method -> b
# Empty where clause
broken_fn2 : a -> b where # Referencing undefined type variable
broken_fn3 : a -> b where module(c)..method : c -> d
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
**INVALID WHERE CONSTRAINT**
Invalid where clause constraint syntax.
Where clauses should contain valid ability constraints.

**where_clauses_error_cases.md:6:11:6:21:**
```roc
    module(a).method -> b
```
          ^^^^^^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "broken_fn1"))
    (type type_14)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "broken_fn2"))
    (type type_34)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 37
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
~~~
# TYPES
~~~roc
broken_fn1 : _e
broken_fn2 : _e
~~~
