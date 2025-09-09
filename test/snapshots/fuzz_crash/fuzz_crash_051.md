# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}{0      0)(0}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseCurly OpenCurly Int Int CloseRound OpenRound Int CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (malformed)
))
(block
  (block
    (num_literal_i32 0)
    (num_literal_i32 0)
    (apply_anon
      (malformed)
      (num_literal_i32 0)
    )
  )
)
~~~
# FORMATTED
~~~roc
module [}]

{
	0
	0
	)(0)
}
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_051.md:1:8:1:9
PARSE ERROR - fuzz_crash_051.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_051.md:1:8:1:9:**
```roc
module[}{0      0)(0}
```
       ^


**PARSE ERROR**
A parsing error occurred: **header_expected_close_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_051.md:1:1:1:9:**
```roc
module[}{0      0)(0}
```
^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_051.md:1:18:1:19:**
```roc
module[}{0      0)(0}
```
                 ^


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_051.md:1:18:1:21:**
```roc
module[}{0      0)(0}
```
                 ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
    (Expr.num_literal_i32 0)
    (Expr.num_literal_i32 0)
    (Expr.fn_call)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 11
(var #0 _)
(var #1 _)
(var #2 Num *)
(var #3 Num *)
(var #4 _)
(var #5 Num *)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 -> #10)
(var #10 fn_pure)
~~~
# TYPES
~~~roc
~~~
