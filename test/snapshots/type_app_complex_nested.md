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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound Comma UpperIdent OpenRound UpperIdent Comma UpperIdent OpenRound LowerIdent CloseRound CloseRound CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow OpenSquare CloseSquare UpperIdent OpenRound Underscore CloseRound OpFatArrow OpenSquare CloseSquare CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound UpperIdent Comma LowerIdent CloseRound CloseRound Comma LowerIdent CloseRound CloseRound OpArrow LowerIdent LowerIdent OpAssign OpBar Underscore OpBar OpenCurly KwCrash String CloseCurly BlankLine LineComment UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent OpenRound LowerIdent CloseRound CloseRound Comma UpperIdent OpenRound UpperIdent Comma UpperIdent OpenRound LowerIdent CloseRound CloseRound CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound UpperIdent OpenRound OpenSquare UpperIdent OpenRound Int CloseRound Comma UpperIdent CloseSquare CloseRound CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

processComplex : Result(List Maybe a, Dict(Str, Error _b)) -> List a
processComplex = |result| match result
	Ok(maybeList) => []
	Err(_) => []

deepNested : Maybe Result(List Dict(Str, a), _b) -> a
deepNested = |_| {
	crash "not implemented"
}

ComplexType((a, b)) : Result(List Maybe a, Dict(Str, Error b))
main! = |_| processComplex(Ok([Some(42), None]))# Test complex nested type applications in function signatures
# Test multiple levels of nesting
# Test type alias with complex nesting
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_app_complex_nested.md:7:9:7:28:**
```roc
        Ok(maybeList) => []
```
        ^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**type_app_complex_nested.md:8:9:8:15:**
```roc
        Err(_) => []
```
        ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "processComplex")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "processComplex")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "deepNested")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.lookup "a")
    )
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
