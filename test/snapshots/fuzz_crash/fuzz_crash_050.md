# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]_0={
)
 
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare Underscore Int OpAssign OpenCurly CloseRound ~~~
# PARSE
~~~clojure
(module-header)
(block
  (underscore)
  (binop_equals
    (num_literal_i32 0)
    (block
      (malformed)
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

_
0 = {
	)
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_050.md:2:1:2:2
PARSE ERROR - fuzz_crash_050.md:4:1:4:1
UNRECOGNIZED SYNTAX - fuzz_crash_050.md:2:1:2:2
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
 ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_050.md:2:1:3:2:**
```roc
)
 
```


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_050.md:1:12:3:2:**
```roc
module[]_0={
)
 
```


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**fuzz_crash_050.md:1:9:1:10:**
```roc
module[]_0={
```
        ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Stmt.assign
    (pattern (Patt.num_literal_i32))
    (Expr.block
      (Expr.malformed)
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 9
(var #0 _)
(var #1 _)
(var #2 -> #4)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
~~~
# TYPES
~~~roc
~~~
