# META
~~~ini
description=Test crash and ellipsis canonicalization
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar Underscore OpBar TripleDot LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar Underscore OpBar OpenCurly KwCrash String CloseCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar Underscore OpBar OpenCurly KwCrash String CloseCurly LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound OpenSquare CloseSquare CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "testEllipsis")
    (binop_thin_arrow
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
    (binop_thin_arrow
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "testCrash")
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
    (lc "testCrashSimple")
    (binop_thin_arrow
      (uc "U64")
      (uc "U64")
    )
  )
  (binop_equals
    (lc "testCrashSimple")
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
app
{
	pf: "../basic-cli/platform.roc" platform [
		main,
	],
}

testEllipsis: (U64 -> U64)
testEllipsis = \_ -> ...

# Test crash statement
testCrash: (U64 -> U64)
testCrash = \_ -> {
	crash "This is a crash message"
}

# Test crash with different message
testCrashSimple: (U64 -> U64)
testCrashSimple = \_ -> {
	crash "oops"
}

main! = \_ -> {
	result1 = testEllipsis(42)
	result2 = testCrash(42)
	result3 = testCrashSimple(42)
	[]
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 4:16 to 4:26

**Unsupported Node**
at 5:16 to 5:20

**Unsupported Node**
at 8:13 to 8:23

**Unsupported Node**
at 9:13 to 9:17

**Unsupported Node**
at 14:19 to 14:29

**Unsupported Node**
at 15:19 to 15:23

**Unsupported Node**
at 19:1 to 19:6

**Unsupported Node**
at 19:9 to 19:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "testEllipsis")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "testCrash")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "testCrashSimple")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
