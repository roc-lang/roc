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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound Comma UpperIdent OpenRound UpperIdent Comma UpperIdent OpenRound LowerIdent CloseRound CloseRound CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow OpenSquare CloseSquare UpperIdent OpenRound Underscore CloseRound OpFatArrow OpenSquare CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound UpperIdent Comma LowerIdent CloseRound CloseRound Comma LowerIdent CloseRound CloseRound OpArrow LowerIdent LowerIdent OpAssign OpBar Underscore OpBar OpenCurly KwCrash String CloseCurly UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound Comma UpperIdent OpenRound UpperIdent Comma UpperIdent OpenRound LowerIdent CloseRound CloseRound CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound UpperIdent OpenRound OpenSquare UpperIdent OpenRound Int CloseRound Comma UpperIdent CloseSquare CloseRound CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "processComplex")
    (binop_thin_arrow
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
        (match <48 branches>)
      )
      (args
        (lc "result")
      )
    )
  )
  (binop_equals
    (lc "deepNested")
    (lambda
      (body
        (block
          (crash <statement>)
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
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (apply_lc
      (lc "processComplex")
      (apply_uc
        (uc "Ok")
        (list_literal
          (tuple_literal
            (apply_uc
              (uc "Some")
              (num_literal_i32 42)
            )
            (uc "None")
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
UNDECLARED TYPE - type_app_complex_nested.md:18:33:18:38
UNDECLARED TYPE - type_app_complex_nested.md:18:54:18:59
UNDECLARED TYPE - type_app_complex_nested.md:4:30:4:35
UNDECLARED TYPE - type_app_complex_nested.md:4:51:4:56
UNUSED VARIABLE - type_app_complex_nested.md:7:12:7:21
UNDECLARED TYPE - type_app_complex_nested.md:12:14:12:19
TYPE MISMATCH - type_app_complex_nested.md:12:55:12:56
# PROBLEMS
**Parse Error**
at 6:5 to 6:18

**Parse Error**
at 7:23 to 7:23

**Parse Error**
at 8:16 to 8:16

**Parse Error**
at 6:5 to 12:1

**Parse Error**
at 20:7 to 20:7

**Unsupported Node**
at 4:18 to 4:73

**Unsupported Node**
at 5:18 to 6:5

**Unsupported Node**
at 13:14 to 13:18

**Unsupported Node**
at 20:5 to 20:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "processComplex")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.lookup "main")
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
