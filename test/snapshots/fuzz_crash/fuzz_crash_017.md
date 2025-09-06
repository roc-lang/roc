# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
me = "luc"
foo = "hello ${namF
~~~
# TOKENS
~~~text
LowerIdent OpAssign String LowerIdent OpAssign MalformedString ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "me")
    (str_literal_small "luc")
  )
  (binop_equals
    (lc "foo")
    (malformed)
  )
)
~~~
# FORMATTED
~~~roc
me = "luc"
foo = "hello ${namF
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_017.md:1:1:1:3
PARSE ERROR - fuzz_crash_017.md:1:4:1:5
PARSE ERROR - fuzz_crash_017.md:1:6:1:7
PARSE ERROR - fuzz_crash_017.md:1:7:1:10
PARSE ERROR - fuzz_crash_017.md:1:10:1:11
PARSE ERROR - fuzz_crash_017.md:2:7:2:8
UNRECOGNIZED SYNTAX - fuzz_crash_017.md:2:7:2:20
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **"hello ${namF** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_017.md:2:7:2:20:**
```roc
foo = "hello ${namF
```
      ^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "me"))
    (Expr.str_literal_small)
  )
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 9
(var #0 _)
(var #1 -> #2)
(var #2 Str)
(var #3 _)
(var #4 -> #8)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
~~~
# TYPES
~~~roc
me : Str
foo : _a
~~~
