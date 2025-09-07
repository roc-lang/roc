# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module [Maybe, some1, none1, some2, none2]

Maybe(a) := [Some(a), None]

some1 : a -> Maybe(a)
some1 = |a| Maybe.Some(a)

none1 : Maybe(_a)
none1 = Maybe.None

some2 = |a| Maybe.Some(a)

none2 = Maybe.None
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColonEqual OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent CloseSquare BlankLine LowerIdent OpColon LowerIdent OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot UpperIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpAssign UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Maybe")

    (lc "some1")

    (lc "none1")

    (lc "some2")

    (lc "none2")
))
(block
  (binop_colon_equals
    (apply_uc
      (uc "Maybe")
      (lc "a")
    )
    (list_literal
      (apply_uc
        (uc "Some")
        (lc "a")
      )
      (uc "None")
    )
  )
  (binop_colon
    (lc "some1")
    (binop_arrow_call
      (lc "a")
      (apply_uc
        (uc "Maybe")
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "some1")
    (lambda
      (body
        (apply_anon
          (binop_dot
            (uc "Maybe")
            (uc "Some")
          )
          (lc "a")
        )
      )
      (args
        (lc "a")
      )
    )
  )
  (binop_colon
    (lc "none1")
    (apply_uc
      (uc "Maybe")
      (lc "_a")
    )
  )
  (binop_equals
    (lc "none1")
    (binop_dot
      (uc "Maybe")
      (uc "None")
    )
  )
  (binop_equals
    (lc "some2")
    (lambda
      (body
        (apply_anon
          (binop_dot
            (uc "Maybe")
            (uc "Some")
          )
          (lc "a")
        )
      )
      (args
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "none2")
    (binop_dot
      (uc "Maybe")
      (uc "None")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [Maybe, some1, none1, some2, none2]

Maybe(a) := [Some(a), None]
some1 : a -> Maybe a
some1 = |a| Maybe.Some(a)
none1 : Maybe _a
none1 = (Maybe.None)
some2 = |a| Maybe.Some(a)
none2 = (Maybe.None)
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**nominal_tag_payload.md:6:1:6:6:**
```roc
some1 = |a| Maybe.Some(a)
```
^^^^^


**SHADOWING**
This definition shadows an existing one.

**nominal_tag_payload.md:9:1:9:6:**
```roc
none1 = Maybe.None
```
^^^^^


**SHADOWING**
This definition shadows an existing one.

**nominal_tag_payload.md:11:10:11:11:**
```roc
some2 = |a| Maybe.Some(a)
```
         ^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.opaque_type)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "some1"))
    (type type_20)
  )
  (Stmt.assign
    (pattern (Patt.ident "some1"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "none1"))
    (type type_34)
  )
  (Stmt.assign
    (pattern (Patt.ident "none1"))
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "some2"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "none2"))
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 66
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
(var #22 -> #59)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 -> #57)
(var #27 _)
(var #28 _)
(var #29 -> #59)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 -> #60)
(var #37 _)
(var #38 _)
(var #39 -> #60)
(var #40 _)
(var #41 -> #64)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 -> #62)
(var #46 _)
(var #47 _)
(var #48 -> #64)
(var #49 _)
(var #50 -> #65)
(var #51 _)
(var #52 _)
(var #53 -> #65)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 -> #58)
(var #58 fn_pure)
(var #59 fn_pure)
(var #60 _)
(var #61 _)
(var #62 -> #63)
(var #63 fn_pure)
(var #64 fn_pure)
(var #65 _)
~~~
# TYPES
~~~roc
a : _b
none1 : _b
some1 : _arg -> _ret
~~~
