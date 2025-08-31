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
foo =
"onmo %
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


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_009.md:1:2:1:3:**
```roc
 f{o,
```
 ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_009.md:1:3:4:4:**
```roc
 f{o,
     ]

foo =
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
