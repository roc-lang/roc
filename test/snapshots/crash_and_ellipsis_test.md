# META
~~~ini
description=Test crash and ellipsis canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Test ellipsis placeholder
testEllipsis : U64 -> U64
testEllipsis = |_| ...

# Test crash statement
testCrash : U64 -> U64
testCrash = |_| {
	crash "This is a crash message"
}

# Test crash with different message
testCrashSimple : U64 -> U64
testCrashSimple = |_| {
	crash "oops"
}

main! = |_| {
    result1 = testEllipsis(42)
    result2 = testCrash(42)
    result3 = testCrashSimple(42)
    []
}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar Underscore OpBar TripleDot BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar Underscore OpBar OpenCurly KwCrash String CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar Underscore OpBar OpenCurly KwCrash String CloseCurly BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound OpenSquare CloseSquare CloseCurly ~~~
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
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
(block
  (binop_colon
    (lc "testEllipsis")
    (binop_arrow_call
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "testEllipsis")
    (lambda
      (body
        (ellipsis)
      )
      (args
        (underscore)
      )
    )
  )
  (binop_colon
    (lc "testCrash")
    (binop_arrow_call
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "testCrash")
    (lambda
      (body
        (block
          (crash
            (str_literal_big "This is a crash message")
          )
        )
      )
      (args
        (underscore)
      )
    )
  )
  (binop_colon
    (lc "testCrashSimple")
    (binop_arrow_call
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "testCrashSimple")
    (lambda
      (body
        (block
          (crash
            (str_literal_small "oops")
          )
        )
      )
      (args
        (underscore)
      )
    )
  )
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (block
          (binop_equals
            (lc "result1")
            (apply_lc
              (lc "testEllipsis")
              (num_literal_i32 42)
            )
          )
          (binop_equals
            (lc "result2")
            (apply_lc
              (lc "testCrash")
              (num_literal_i32 42)
            )
          )
          (binop_equals
            (lc "result3")
            (apply_lc
              (lc "testCrashSimple")
              (num_literal_i32 42)
            )
          )
          (list_literal)
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
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

# Test ellipsis placeholder
testEllipsis : U64 -> U64
testEllipsis = |_| ...
# Test crash statement
testCrash : U64 -> U64
testCrash = |_| {
	crash "This is a crash message"
}

# Test crash with different message
testCrashSimple : U64 -> U64
testCrashSimple = |_| {
	crash "oops"
}

main! = |_| {
	result1 = testEllipsis(42)
	result2 = testCrash(42)
	result3 = testCrashSimple(42)
	[]
}
~~~
# EXPECTED
UNUSED VARIABLE - crash_and_ellipsis_test.md:20:5:20:12
UNUSED VARIABLE - crash_and_ellipsis_test.md:21:5:21:12
UNUSED VARIABLE - crash_and_ellipsis_test.md:22:5:22:12
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "testEllipsis"))
    (type type_10)
  )
  (Stmt.assign
    (pattern (Patt.ident "testEllipsis"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "testCrash"))
    (type type_20)
  )
  (Stmt.assign
    (pattern (Patt.ident "testCrash"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "testCrashSimple"))
    (type type_32)
  )
  (Stmt.assign
    (pattern (Patt.ident "testCrashSimple"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 75
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
(var #12 -> #65)
(var #13 _)
(var #14 _)
(var #15 -> #65)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 -> #67)
(var #23 _)
(var #24 Str)
(var #25 _)
(var #26 _)
(var #27 -> #67)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 -> #69)
(var #35 _)
(var #36 Str)
(var #37 _)
(var #38 _)
(var #39 -> #69)
(var #40 _)
(var #41 -> #74)
(var #42 _)
(var #43 -> #46)
(var #44 -> #71)
(var #45 Num *)
(var #46 _)
(var #47 _)
(var #48 -> #51)
(var #49 -> #72)
(var #50 Num *)
(var #51 _)
(var #52 _)
(var #53 -> #56)
(var #54 -> #73)
(var #55 Num *)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 -> #74)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 fn_pure)
(var #66 _)
(var #67 fn_pure)
(var #68 _)
(var #69 fn_pure)
(var #70 _)
(var #71 fn_pure)
(var #72 fn_pure)
(var #73 fn_pure)
(var #74 fn_pure)
~~~
# TYPES
~~~roc
result2 : _a
testCrashSimple : _arg -> _ret
testCrash : _arg -> _ret
testEllipsis : _arg -> _ret
main : _arg -> _ret
result3 : _a
result1 : _a
~~~
