# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{
o:0)
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpenCurly LowerIdent OpColon Int CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "f")
      (binop_platform
        (str_literal_small "")
        (block)
      )
    )
))
(block
  (block
    (binop_colon
      (lc "o")
      (num_literal_i32 0)
    )
    (malformed)
  )
)
~~~
# FORMATTED
~~~roc
app { f: "" platform [] }

{
	o : 0
	)
}
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_040.md:1:20:1:21
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_040.md:2:3:2:4
PARSE ERROR - fuzz_crash_040.md:2:4:2:5
MALFORMED TYPE - fuzz_crash_040.md:2:3:2:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_040.md:2:4:2:5:**
```roc
o:0)
```
   ^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_040.md:1:20:2:5:**
```roc
app[]{f:platform""}{
o:0)
```


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**fuzz_crash_040.md:2:3:2:4:**
```roc
o:0)
```
  ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
    (Stmt.standalone_type_anno
      (pattern (Patt.ident "o"))
      (type type_7)
    )
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 13
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
~~~
# TYPES
~~~roc
o : _a
~~~
