# META
~~~ini
description=Annotated effectful function
type=expr
~~~
# SOURCE
~~~roc
{
    launchTheNukes : {} => Result Bool LaunchNukeErr
    launchTheNukes = |{}| ...

    launchTheNukes({})
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon OpenCurly CloseCurly OpThinArrow UpperIdent UpperIdent UpperIdent LowerIdent OpAssign OpBar OpenCurly CloseCurly OpBar TripleDot BlankLine LowerIdent OpenRound OpenCurly CloseCurly CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "launchTheNukes")
    (binop_thick_arrow
      (record_literal)
      (uc "Result")
    )
  )
  (uc "Bool")
  (uc "LaunchNukeErr")
  (binop_equals
    (lc "launchTheNukes")
    (lambda
      (body
        (ellipsis)
      )
      (args
        (record_literal)
      )
    )
  )
  (apply_lc
    (lc "launchTheNukes")
    (record_literal)
  )
)
~~~
# FORMATTED
~~~roc
launchTheNukes : {} => Result
Bool
LaunchNukeErr
launchTheNukes = |{}| ...
launchTheNukes({})
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**ann_effectful_fn.md:3:5:3:19:**
```roc
    launchTheNukes = |{}| ...
```
    ^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "launchTheNukes"))
    (type type_4)
  )
  (Expr.tag_no_args)
  (Expr.tag_no_args)
  (Stmt.assign
    (pattern (Patt.ident "launchTheNukes"))
    (Expr.lambda (canonicalized))
  )
  (Expr.fn_call)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 22
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 -> #19)
(var #9 _)
(var #10 _)
(var #11 -> #19)
(var #12 _)
(var #13 -> #21)
(var #14 -> #20)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 fn_pure)
(var #20 {})
(var #21 fn_pure)
~~~
# TYPES
~~~roc
launchTheNukes : _arg -> _ret
~~~
