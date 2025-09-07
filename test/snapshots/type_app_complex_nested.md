# META
~~~ini
description=Complex nested type applications in function annotation - focused test
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Test complex nested type applications in function signatures
processComplex : Result(List(Maybe(a)), Dict(Str, Error(_b))) -> List(a)
processComplex = |result|
    match result {
        Ok(maybeList) => []
        Err(_) => []
    }

# Test multiple levels of nesting
deepNested : Maybe(Result(List(Dict(Str, a)), _b)) -> a
deepNested = |_| {
	crash "not implemented"
}

# Test type alias with complex nesting
ComplexType(a, b) : Result(List(Maybe(a)), Dict(Str, Error(b)))

main! = |_| processComplex(Ok([Some(42), None]))
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound Comma UpperIdent OpenRound UpperIdent Comma UpperIdent OpenRound LowerIdent CloseRound CloseRound CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow OpenSquare CloseSquare UpperIdent OpenRound Underscore CloseRound OpFatArrow OpenSquare CloseSquare CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound UpperIdent Comma LowerIdent CloseRound CloseRound Comma LowerIdent CloseRound CloseRound OpArrow LowerIdent LowerIdent OpAssign OpBar Underscore OpBar OpenCurly KwCrash String CloseCurly BlankLine LineComment UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound Comma UpperIdent OpenRound UpperIdent Comma UpperIdent OpenRound LowerIdent CloseRound CloseRound CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound UpperIdent OpenRound OpenSquare UpperIdent OpenRound Int CloseRound Comma UpperIdent CloseSquare CloseRound CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block)
      )
    )
))
(block
  (binop_colon
    (lc "processComplex")
    (binop_arrow_call
      (apply_uc
        (uc "Result")
        (tuple_literal
          (apply_uc
            (uc "List")
            (apply_uc
              (uc "Maybe")
              (lc "a")
            )
          )
          (apply_uc
            (uc "Dict")
            (tuple_literal
              (uc "Str")
              (apply_uc
                (uc "Error")
                (lc "_b")
              )
            )
          )
        )
      )
      (apply_uc
        (uc "List")
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "processComplex")
    (lambda
      (body
        (match
          (scrutinee             (lc "result")
)
          (branch1             (binop_thick_arrow
              (apply_uc
                (uc "Ok")
                (lc "maybeList")
              )
              (list_literal)
            )
)
          (branch2             (binop_thick_arrow
              (apply_uc
                (uc "Err")
                (underscore)
              )
              (list_literal)
            )
))
      )
      (args
        (lc "result")
      )
    )
  )
  (binop_colon
    (lc "deepNested")
    (binop_arrow_call
      (apply_uc
        (uc "Maybe")
        (apply_uc
          (uc "Result")
          (tuple_literal
            (apply_uc
              (uc "List")
              (apply_uc
                (uc "Dict")
                (tuple_literal
                  (uc "Str")
                  (lc "a")
                )
              )
            )
            (lc "_b")
          )
        )
      )
      (lc "a")
    )
  )
  (binop_equals
    (lc "deepNested")
    (lambda
      (body
        (block
          (crash
            (str_literal_big "not implemented")
          )
        )
      )
      (args
        (underscore)
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "ComplexType")
      (tuple_literal
        (lc "a")
        (lc "b")
      )
    )
    (apply_uc
      (uc "Result")
      (tuple_literal
        (apply_uc
          (uc "List")
          (apply_uc
            (uc "Maybe")
            (lc "a")
          )
        )
        (apply_uc
          (uc "Dict")
          (tuple_literal
            (uc "Str")
            (apply_uc
              (uc "Error")
              (lc "b")
            )
          )
        )
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (apply_lc
          (lc "processComplex")
          (apply_uc
            (uc "Ok")
            (list_literal
              (apply_uc
                (uc "Some")
                (num_literal_i32 42)
              )
              (uc "None")
            )
          )
        )
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
app [main!] { pf: "../basic-cli/main.roc" platform [] }

# Test complex nested type applications in function signatures
processComplex : Result(List Maybe a, Dict(Str, Error _b)) -> List a
processComplex = |result| match result
	Ok(maybeList) => []
	Err(_) => []

# Test multiple levels of nesting
deepNested : Maybe Result(List Dict(Str, a), _b) -> a
deepNested = |_| {
	crash "not implemented"
}

# Test type alias with complex nesting
ComplexType((a, b)) : Result(List Maybe a, Dict(Str, Error b))
main! = |_| processComplex(Ok([Some(42), None]))
~~~
# EXPECTED
UNDECLARED TYPE - type_app_complex_nested.md:18:33:18:38
UNDECLARED TYPE - type_app_complex_nested.md:18:54:18:59
UNDECLARED TYPE - type_app_complex_nested.md:4:30:4:35
UNDECLARED TYPE - type_app_complex_nested.md:4:51:4:56
UNUSED VARIABLE - type_app_complex_nested.md:7:12:7:21
UNDECLARED TYPE - type_app_complex_nested.md:12:14:12:19
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**type_app_complex_nested.md:4:1:4:15:**
```roc
processComplex : Result(List(Maybe(a)), Dict(Str, Error(_b))) -> List(a)
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_app_complex_nested.md:5:1:5:15:**
```roc
processComplex = |result|
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_app_complex_nested.md:12:1:12:11:**
```roc
deepNested : Maybe(Result(List(Dict(Str, a)), _b)) -> a
```
^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**type_app_complex_nested.md:13:1:13:11:**
```roc
deepNested = |_| {
```
^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "processComplex"))
    (type type_26)
  )
  (Stmt.assign
    (pattern (Patt.ident "processComplex"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "deepNested"))
    (type type_59)
  )
  (Stmt.assign
    (pattern (Patt.ident "deepNested"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_alias)
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 111
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
(var #28 -> #104)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 -> #104)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 -> #106)
(var #62 _)
(var #63 Str)
(var #64 _)
(var #65 _)
(var #66 -> #106)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 _)
(var #71 _)
(var #72 _)
(var #73 _)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 _)
(var #81 _)
(var #82 _)
(var #83 _)
(var #84 _)
(var #85 _)
(var #86 _)
(var #87 _)
(var #88 _)
(var #89 -> #110)
(var #90 _)
(var #91 -> #109)
(var #92 -> #108)
(var #93 _)
(var #94 Num *)
(var #95 _)
(var #96 _)
(var #97 _)
(var #98 _)
(var #99 _)
(var #100 -> #110)
(var #101 _)
(var #102 _)
(var #103 _)
(var #104 fn_pure)
(var #105 _)
(var #106 fn_pure)
(var #107 _)
(var #108 fn_pure)
(var #109 fn_pure)
(var #110 fn_pure)
~~~
# TYPES
~~~roc
deepNested : _arg -> _ret
maybeList : _c
processComplex : _arg -> _ret
main : _arg -> _ret
result : _c
~~~
