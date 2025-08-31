# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
 f{o,
     ]

foo =

    "onmo %
~~~
# TOKENS
~~~text
LowerIdent OpenCurly LowerIdent Comma CloseSquare BlankLine LowerIdent OpAssign BlankLine MalformedString ~~~
# PARSE
~~~clojure
(block
  (lc "f")
  (record_literal
    (lc "o")
    (malformed malformed:expr_unexpected_token)
  )
  (binop_equals
    (lc "foo")
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
f
{
	o,
}
foo = "onmo %
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_009.md:2:6:4:1:**
```roc
     ]

foo =
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_009.md:1:3:4:1:**
```roc
 f{o,
     ]

foo =
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"onmo %** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_009.md:6:5:6:12:**
```roc
    "onmo %
```
    ^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "f")
  (Expr.record_literal
    (Expr.lookup "o")
    (Expr.malformed)
  )
  (Expr.binop_equals
    (Expr.lookup "foo")
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
foo : Error
~~~
