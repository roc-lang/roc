# META
~~~ini
description=Complex nested type applications in function annotation - focused test
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound Comma UpperIdent OpenRound UpperIdent Comma UpperIdent OpenRound LowerIdent CloseRound CloseRound CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow OpenSquare CloseSquare UpperIdent OpenRound Underscore CloseRound OpFatArrow OpenSquare CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound UpperIdent Comma LowerIdent CloseRound CloseRound Comma LowerIdent CloseRound CloseRound OpArrow LowerIdent LowerIdent OpAssign OpBar Underscore OpBar OpenCurly KwCrash String CloseCurly UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound Comma UpperIdent OpenRound UpperIdent Comma UpperIdent OpenRound LowerIdent CloseRound CloseRound CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound UpperIdent OpenRound OpenSquare UpperIdent OpenRound Int CloseRound Comma UpperIdent CloseSquare CloseRound CloseRound ~~~
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
        (binop_thin_arrow
          (binop_colon
            (match <0 branches>)
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
          )
          (lc "a")
        )
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
app { pf: "../basic-cli/main.roc" platform [main] }

processComplex : Result(List Maybe a, Dict(Str, Error _b)) -> List a
processComplex = |result| match result
 : Maybe Result(List Dict(Str, a), _b) -> a
deepNested = |_| {
	crash "not implemented"
}
ComplexType((a, b)) : Result(List Maybe a, Dict(Str, Error b))
main! = |_| processComplex(Ok([Some(42), None]))
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 7:23 to 7:26

**Parse Error**
at 8:16 to 8:19

**Parse Error**
at 6:18 to 12:12

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "processComplex")
    (Expr.binop_thin_arrow)
  )
  (Expr.binop_equals
    (Expr.lookup "processComplex")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "deepNested")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
processComplex : _c
deepNested : _c
~~~
