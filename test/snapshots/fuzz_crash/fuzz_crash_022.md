# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app [main!] { |f: platform "c" }

UserId : U64

ser : UserId -> Str
getUser = |id| if (id > 1!) "big" else "l"

-ain! = |_| getUser(900)
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly OpBar LowerIdent OpColon KwPlatform String CloseCurly BlankLine UpperIdent OpColon UpperIdent BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwIf OpenRound LowerIdent OpGreaterThan Int OpBang CloseRound String KwElse String BlankLine OpUnaryMinus LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
))
(block
  (malformed)
  (malformed)
  (str_literal_small "c")
  (malformed)
  (binop_colon
    (uc "UserId")
    (uc "U64")
  )
  (binop_colon
    (lc "ser")
    (binop_arrow_call
      (uc "UserId")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "getUser")
    (lambda
      (body
        (if_without_else
          (condition             (binop_gt
              (lc "id")
              (num_literal_i32 1)
            )
)
          (then             (unary_not <unary_op>)
))
      )
      (args
        (lc "id")
      )
    )
  )
  (str_literal_small "big")
  (malformed)
  (binop_equals
    (binop_minus
      (str_literal_small "l")
      (not_lc "ain")
    )
    (lambda
      (body
        (apply_lc
          (lc "getUser")
          (num_literal_i32 900)
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
app [main!] { }

: 
platform 
"c"
}

UserId : U64
ser : UserId -> Str
getUser = |id| if id > 1 !) 
"big"
else 
"l" - ain! = |_| getUser(900)
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_022.md:1:1:1:4
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_022.md:1:19:1:27
PARSE ERROR - fuzz_crash_022.md:1:28:1:29
PARSE ERROR - fuzz_crash_022.md:1:29:1:30
PARSE ERROR - fuzz_crash_022.md:1:30:1:31
PARSE ERROR - fuzz_crash_022.md:1:32:1:33
PARSE ERROR - fuzz_crash_022.md:6:27:6:28
PARSE ERROR - fuzz_crash_022.md:8:1:8:2
MALFORMED TYPE - fuzz_crash_022.md:1:19:1:27
INVALID IF CONDITION - :0:0:0:0
UNUSED VARIABLE - fuzz_crash_022.md:6:12:6:14
# PROBLEMS
**EXPECTED PACKAGE OR PLATFORM NAME**
A parsing error occurred: **expected_package_or_platform_name**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_022.md:1:1:1:15:**
```roc
app [main!] { |f: platform "c" }
```
^^^^^^^^^^^^^^


**EXPECTED CLOSE CURLY BRACE**
A parsing error occurred: **expected_package_platform_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_022.md:1:1:1:15:**
```roc
app [main!] { |f: platform "c" }
```
^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_022.md:1:17:1:19:**
```roc
app [main!] { |f: platform "c" }
```
                ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **platform ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_022.md:1:19:1:28:**
```roc
app [main!] { |f: platform "c" }
```
                  ^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_022.md:1:32:3:1:**
```roc
app [main!] { |f: platform "c" }

UserId : U64
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **) ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_022.md:6:27:6:29:**
```roc
getUser = |id| if (id > 1!) "big" else "l"
```
                          ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **else ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_022.md:6:35:6:40:**
```roc
getUser = |id| if (id > 1!) "big" else "l"
```
                                  ^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_022.md:5:1:5:4:**
```roc
ser : UserId -> Str
```
^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_022.md:6:1:6:8:**
```roc
getUser = |id| if (id > 1!) "big" else "l"
```
^^^^^^^


**EXPRESSION IN PATTERN CONTEXT**
Found an expression where a pattern was expected.
This location requires a pattern for matching or destructuring, not a computed value.

**fuzz_crash_022.md:6:40:8:6:**
```roc
getUser = |id| if (id > 1!) "big" else "l"

-ain! = |_| getUser(900)
```


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "ser"))
    (type type_13)
  )
  (Stmt.assign
    (pattern (Patt.ident "getUser"))
    (Expr.lambda (canonicalized))
  )
  (Expr.str_literal_small)
  (Expr.malformed)
  (Stmt.assign
    (pattern (Patt.malformed))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 46
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 Str)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 -> #41)
(var #16 _)
(var #17 _)
(var #18 Num *)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 -> #41)
(var #24 _)
(var #25 Str)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 -> #45)
(var #30 _)
(var #31 -> #44)
(var #32 Num *)
(var #33 _)
(var #34 -> #45)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 fn_pure)
(var #42 _)
(var #43 _)
(var #44 fn_pure)
(var #45 fn_pure)
~~~
# TYPES
~~~roc
ser : _a
getUser : _arg -> _ret
id : _a
~~~
