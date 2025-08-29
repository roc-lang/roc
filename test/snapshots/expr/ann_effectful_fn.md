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
OpenCurly LowerIdent OpColon OpenCurly CloseCurly OpFatArrow UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpAssign OpBar OpenCurly CloseCurly OpBar TripleDot LowerIdent OpenRound OpenCurly CloseCurly CloseRound CloseCurly ~~~
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
launchTheNukes : {  } => Result((Bool, LaunchNukeErr))
launchTheNukes = |{  }| ...
launchTheNukes({  })
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:22 to 2:24

**Unsupported Node**
at 3:23 to 3:25

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "launchTheNukes")
    (Expr.binop_thick_arrow)
  )
  (Expr.binop_equals
    (Expr.lookup "launchTheNukes")
    (Expr.lambda)
  )
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
