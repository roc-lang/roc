# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}0}.a
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseCurly Int CloseCurly Dot LowerIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (malformed)
))
(block
  (num_literal_i32 0)
  (binop_pipe
    (malformed)
    (dot_lc "a")
  )
)
~~~
# FORMATTED
~~~roc
module [}]

0
} | .a
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_063.md:1:8:1:9
PARSE ERROR - fuzz_crash_063.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_063.md:1:8:1:9:**
```roc
module[}0}.a
```
       ^


**PARSE ERROR**
A parsing error occurred: **header_expected_close_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_063.md:1:1:1:9:**
```roc
module[}0}.a
```
^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_063.md:1:10:1:11:**
```roc
module[}0}.a
```
         ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.num_literal_i32 0)
  (Expr.binop_pipe)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 7
(var #0 _)
(var #1 _)
(var #2 Num *)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
~~~
# TYPES
~~~roc
~~~
