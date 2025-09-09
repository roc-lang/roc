# META
~~~ini
description=Module dispatch in where clause
type=file
~~~
# SOURCE
~~~roc
module [deserialize]

deserialize : List(U8) -> Result(a, [DecodeErr])
	where module(a).decode : List(U8) -> Result(a, [DecodeErr])
deserialize = |_| ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma OpenSquare UpperIdent CloseSquare CloseRound KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma OpenSquare UpperIdent CloseSquare CloseRound LowerIdent OpAssign OpBar Underscore OpBar TripleDot ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "deserialize")
))
(block
  (binop_colon
    (lc "deserialize")
    (binop_arrow_call
      (binop_where
        (binop_arrow_call
          (apply_uc
            (uc "List")
            (uc "U8")
          )
          (apply_uc
            (uc "Result")
            (tuple_literal
              (lc "a")
              (list_literal
                (uc "DecodeErr")
              )
            )
          )
        )
        (binop_colon
          (binop_dot
            (apply_module
              (lc "a")
            )
            (dot_lc "decode")
          )
          (apply_uc
            (uc "List")
            (uc "U8")
          )
        )
      )
      (apply_uc
        (uc "Result")
        (tuple_literal
          (lc "a")
          (list_literal
            (uc "DecodeErr")
          )
        )
      )
    )
  )
  (binop_equals
    (lc "deserialize")
    (lambda
      (body
        (ellipsis)
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [deserialize]

deserialize : List U8 -> Result(a, [DecodeErr]) where module(a)..decode : List U8 -> Result(a, [DecodeErr])
deserialize = |_| ...
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**where_clauses_serde_example.md:5:1:5:12:**
```roc
deserialize = |_| ...
```
^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "deserialize"))
    (type type_28)
  )
  (Stmt.assign
    (pattern (Patt.ident "deserialize"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 39
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
(var #30 -> #38)
(var #31 _)
(var #32 _)
(var #33 -> #38)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 fn_pure)
~~~
# TYPES
~~~roc
deserialize : _arg -> _ret
~~~
