# META
~~~ini
description=Type shadowing across scopes should produce warning
type=file
~~~
# SOURCE
~~~roc
module [Result, processData]

Result(a, b) : [Ok(a), Err(b)]

processData : Str -> Str
processData = |data|
    "processed"

# In a nested module scope, redeclare Result
InnerModule : {
    Result : [Success, Failure]
}
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String UpperIdent OpColon OpenCurly UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent CloseSquare CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (apply_uc
      (uc "Result")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (list_literal
      (tuple_literal
        (apply_uc
          (uc "Ok")
          (lc "a")
        )
        (apply_uc
          (uc "Err")
          (lc "b")
        )
      )
    )
  )
  (binop_colon
    (lc "processData")
    (binop_thin_arrow
      (uc "Str")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "processData")
    (lambda
      (body
        (str_literal_big "processed")
      )
      (args
        (lc "data")
      )
    )
  )
  (binop_colon
    (uc "InnerModule")
    (block
      (binop_colon
        (uc "Result")
        (list_literal
          (tuple_literal
            (uc "Success")
            (uc "Failure")
          )
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - type_shadowing_across_scopes.md:11:5:11:11
PARSE ERROR - type_shadowing_across_scopes.md:11:24:11:31
PARSE ERROR - type_shadowing_across_scopes.md:11:31:11:32
PARSE ERROR - type_shadowing_across_scopes.md:12:1:12:2
TYPE REDECLARED - type_shadowing_across_scopes.md:3:1:3:31
MALFORMED TYPE - type_shadowing_across_scopes.md:11:24:11:31
UNUSED VARIABLE - type_shadowing_across_scopes.md:6:16:6:20
# PROBLEMS
**Unsupported Node**
at 3:16 to 4:1

**Unsupported Node**
at 5:15 to 5:25

**Unsupported Node**
at 6:15 to 7:5

**Pattern in Expression Context**
at 10:1 to 10:12

**Pattern in Expression Context**
at 11:5 to 11:11

**Unsupported Node**
at 11:14 to 12:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "processData")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.malformed)
        (Expr.malformed)
      )
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
