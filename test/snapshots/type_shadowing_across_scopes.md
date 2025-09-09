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
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment UpperIdent OpColon OpenCurly UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent CloseSquare CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Result")

    (lc "processData")
))
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
  (binop_colon
    (lc "processData")
    (binop_arrow_call
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
          (uc "Success")
          (uc "Failure")
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [Result, processData]

Result((a, b)) : [Ok(a), Err(b)]
processData : Str -> Str
processData = |data| "processed"
# In a nested module scope, redeclare Result
InnerModule : {
	Result : [Success, Failure]
}
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
**SHADOWING**
This definition shadows an existing one.

**type_shadowing_across_scopes.md:6:1:6:12:**
```roc
processData = |data|
```
^^^^^^^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "processData"))
    (type type_19)
  )
  (Stmt.assign
    (pattern (Patt.ident "processData"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_alias)
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
(var #21 -> #36)
(var #22 _)
(var #23 Str)
(var #24 -> #36)
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
(var #36 fn_pure)
~~~
# TYPES
~~~roc
data : _c
processData : _arg -> Str
~~~
