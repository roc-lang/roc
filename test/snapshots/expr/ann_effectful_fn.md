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
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**ann_effectful_fn.md:2:22:2:24:**
```roc
    launchTheNukes : {} => Result(Bool, LaunchNukeErr)
```
                     ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**ann_effectful_fn.md:3:23:3:25:**
```roc
    launchTheNukes = |{}| ...
```
                      ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "launchTheNukes")
    (Expr.binop_thick_arrow
      (Expr.malformed)
      (Expr.apply_tag)
    )
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
