# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app { |f: "c" platform [main!] }

UserId : U64

ser : UserId -> Str
getUser = |id| if (id > 1!) "big" else "l"

-ain! = |_| getUser(900)
~~~
# TOKENS
~~~text
KwApp OpenCurly OpBar LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine UpperIdent OpColon UpperIdent BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwIf OpenRound LowerIdent OpGreaterThan Int OpBang CloseRound String KwElse String BlankLine OpUnaryMinus LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(app-header)
~~~
# FORMATTED
~~~roc
app { }

: 
"c"
platform 
[main!]
}

UserId : U64
ser : UserId -> Str
getUser = |id| if id > 1 !
) 
"big"
else 
"l" - ain! = |_| getUser(900)
~~~
# EXPECTED
NIL
# PROBLEMS
**EXPECTED PACKAGE OR PLATFORM NAME**
A parsing error occurred: **expected_package_or_platform_name**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_022.md:1:1:1:7:**
```roc
app { |f: "c" platform [main!] }
```
^^^^^^


**EXPECTED CLOSE CURLY BRACE**
A parsing error occurred: **expected_package_platform_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_022.md:1:1:1:7:**
```roc
app { |f: "c" platform [main!] }
```
^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_022.md:1:9:1:11:**
```roc
app { |f: "c" platform [main!] }
```
        ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **platform ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_022.md:1:15:1:24:**
```roc
app { |f: "c" platform [main!] }
```
              ^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_022.md:1:32:3:1:**
```roc
app { |f: "c" platform [main!] }

UserId : U64
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **!** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_022.md:6:26:6:27:**
```roc
getUser = |id| if (id > 1!) "big" else "l"
```
                         ^


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


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.list_literal)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "ser")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "getUser")
    (Expr.lambda)
  )
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.binop_minus
      (Expr.str_literal_small)
      (Expr.not_lookup)
    )
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
getUser : _a
~~~
