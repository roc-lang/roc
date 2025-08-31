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


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "processComplex")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "processComplex"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "deepNested")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "deepNested"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name node:apply_uc)
    (type apply_uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
