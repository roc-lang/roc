# META
~~~ini
description=Annotated effectful function
type=expr
~~~
# SOURCE
~~~roc
{
    launchTheNukes : {} => Result(Bool, LaunchNukeErr)
    launchTheNukes = |{}| ...

    launchTheNukes({})
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon OpenCurly CloseCurly OpFatArrow UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpAssign OpBar OpenCurly CloseCurly OpBar TripleDot BlankLine LowerIdent OpenRound OpenCurly CloseCurly CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "launchTheNukes")
    (binop_thick_arrow
      (record_literal)
      (apply_uc
        (uc "Result")
        (tuple_literal
          (uc "Bool")
          (uc "LaunchNukeErr")
        )
      )
    )
  )
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
launchTheNukes : {} => Result((Bool, LaunchNukeErr))
launchTheNukes = |{}| ...
launchTheNukes({})
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
launchTheNukes : _a
~~~
