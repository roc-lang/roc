# META
~~~ini
description=Where clause with multiline constraints
type=file
~~~
# SOURCE
~~~roc
module [process]

process : a, b -> c
	where
		module(a).convert : a -> c,
		module(b).transform : b -> c
process = ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign TripleDot ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "process")
))
(block
  (binop_colon
    (lc "process")
    (binop_arrow_call
      (lc "a")
      (binop_arrow_call
        (lc "b")
        (binop_arrow_call
          (binop_colon
            (tuple_literal
              (binop_arrow_call
                (binop_where
                  (lc "c")
                  (binop_colon
                    (binop_pipe
                      (apply_module
                        (lc "a")
                      )
                      (dot_lc "convert")
                    )
                    (lc "a")
                  )
                )
                (lc "c")
              )
              (binop_pipe
                (apply_module
                  (lc "b")
                )
                (dot_lc "transform")
              )
            )
            (lc "b")
          )
          (lc "c")
        )
      )
    )
  )
  (binop_equals
    (lc "process")
    (ellipsis)
  )
)
~~~
# FORMATTED
~~~roc
module [process]

process : a -> b -> (c where module(a).convert : a) -> c, module(b).transform : b -> c
process = ...
~~~
# EXPECTED
NIL
# PROBLEMS
**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**where_clauses_multiline.md:3:19:6:26:**
```roc
process : a, b -> c
	where
		module(a).convert : a -> c,
		module(b).transform : b -> c
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "process"))
    (type type_25)
  )
  (Stmt.assign
    (pattern (Patt.ident "process"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 32
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
(var #27 -> #31)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
~~~
# TYPES
~~~roc
process : _d
~~~
