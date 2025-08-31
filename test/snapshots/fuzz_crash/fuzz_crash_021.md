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
    (binop_pipe
      (lc "main")
      (dot_lc "roc")
    )
  )
  (malformed malformed:expr_unexpected_token)
  (apply_uc
    (uc "Pair")
    (tuple_literal
      (lc "a")
      (binop_plus
        (lc "b")
        (apply_anon
          (malformed malformed:expr_unexpected_token)
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
Fli / main.roc
" }

Pair((a, b + : ()))
~~~
# EXPECTED
NIL
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


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_slash
    (Expr.apply_tag)
    (Expr.lambda)
  )
  (Expr.malformed)
  (Expr.apply_tag)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
