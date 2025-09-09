# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
Fli/main.roc" }

Pair(a, b+ : (
~~~
# TOKENS
~~~text
UpperIdent OpSlash LowerIdent Dot LowerIdent MalformedString BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent OpPlus OpColon OpenRound ~~~
# PARSE
~~~clojure
(block
  (binop_slash
    (uc "Fli")
    (binop_dot
      (lc "main")
      (dot_lc "roc")
    )
  )
  (malformed)
  (apply_uc
    (uc "Pair")
    (tuple_literal
      (lc "a")
      (binop_plus
        (lc "b")
        (apply_anon
          (malformed)
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
Fli / (main..roc)
" }

Pair((a, b + : ()))
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
MISSING HEADER - fuzz_crash_021.md:1:1:1:4
PARSE ERROR - fuzz_crash_021.md:1:4:1:5
PARSE ERROR - fuzz_crash_021.md:1:5:1:9
PARSE ERROR - fuzz_crash_021.md:1:9:1:13
PARSE ERROR - fuzz_crash_021.md:1:13:1:14
PARSE ERROR - fuzz_crash_021.md:1:14:1:16
PARSE ERROR - fuzz_crash_021.md:1:16:1:16
PARSE ERROR - fuzz_crash_021.md:3:1:3:5
PARSE ERROR - fuzz_crash_021.md:4:1:4:1
MALFORMED TYPE - fuzz_crash_021.md:3:14:3:15
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **" }

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_021.md:1:13:3:1:**
```roc
Fli/main.roc" }

Pair(a, b+ : (
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_021.md:3:12:3:14:**
```roc
Pair(a, b+ : (
```
           ^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:3:12:3:15:**
```roc
Pair(a, b+ : (
```
           ^^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_021.md:3:1:3:15:**
```roc
Pair(a, b+ : (
```
^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **main** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_021.md:1:5:1:9:**
```roc
Fli/main.roc" }
```
    ^^^^


**UNDEFINED VARIABLE**
Nothing is named **a** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_021.md:3:6:3:7:**
```roc
Pair(a, b+ : (
```
     ^


**UNDEFINED VARIABLE**
Nothing is named **b** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_021.md:3:9:3:10:**
```roc
Pair(a, b+ : (
```
        ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_slash
    (Expr.tag_no_args)
    (Expr.record_access)
  )
  (Expr.malformed)
  (Expr.tag_applied)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 21
(var #0 _)
(var #1 -> #4)
(var #2 _)
(var #3 _)
(var #4 -> #5)
(var #5 _)
(var #6 _)
(var #7 -> #20)
(var #8 _)
(var #9 -> #11)
(var #10 _)
(var #11 -> #12)
(var #12 _)
(var #13 -> #19)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 -> #18)
(var #18 fn_pure)
(var #19 tuple)
(var #20 fn_pure)
~~~
# TYPES
~~~roc
~~~
