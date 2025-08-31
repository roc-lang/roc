# META
~~~ini
description=Block with pattern unification testing
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    str = "hello"
    result = x + 5
    result
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign String LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "str")
    (str_literal_big "hello")
  )
  (binop_equals
    (lc "result")
    (binop_plus
      (lc "x")
      (num_literal_i32 5)
    )
  )
  (lc "result")
)
~~~
# FORMATTED
~~~roc
x = 42
str = "hello"
result = x + 5
result
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **str** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_str` to suppress this warning.
The unused variable is declared here:

**block_pattern_unify.md:3:5:3:8:**
```roc
    str = "hello"
```
    ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "result")
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
x : Num(_size)
str : Str
result : Num(_size)
~~~
