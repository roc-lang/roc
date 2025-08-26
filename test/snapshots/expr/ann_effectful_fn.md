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
OpenCurly LowerIdent OpColon OpenCurly CloseCurly OpFatArrow UpperIdent UpperIdent UpperIdent LowerIdent OpAssign OpBar OpenCurly CloseCurly OpBar TripleDot LowerIdent OpenRound OpenCurly CloseCurly CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "launchTheNukes")
    (record_literal)
  )
  (malformed malformed:expr_unexpected_token)
  (uc "Result")
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
launchTheNukes : {}
=>
Result
Bool
LaunchNukeErr
launchTheNukes = \{  } -> ...

launchTheNukes({  })
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:25 to 2:25

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.str_literal_small)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.binop_thick_arrow)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
