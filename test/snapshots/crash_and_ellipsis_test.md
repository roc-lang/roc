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
**UNUSED VARIABLE**
Variable **result2** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result2` to suppress this warning.
The unused variable is declared here:

**crash_and_ellipsis_test.md:21:5:21:12:**
```roc
    result2 = testCrash(42)
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable **result3** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result3` to suppress this warning.
The unused variable is declared here:

**crash_and_ellipsis_test.md:22:5:22:12:**
```roc
    result3 = testCrashSimple(42)
```
    ^^^^^^^


**UNUSED VARIABLE**
Variable **result1** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result1` to suppress this warning.
The unused variable is declared here:

**crash_and_ellipsis_test.md:20:5:20:12:**
```roc
    result1 = testEllipsis(42)
```
    ^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "testEllipsis")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "testEllipsis"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "testCrash")
    (type <mutated_tag:161>)
  )
  (Stmt.assign
    (pattern (Patt.ident "testCrash"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "testCrashSimple")
    (type <mutated_tag:161>)
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
~~~
# TYPES
~~~roc
~~~
