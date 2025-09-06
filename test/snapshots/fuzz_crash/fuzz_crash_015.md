# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0o0.0
0_0
0u8.0
0_
~~~
# TOKENS
~~~text
Int Dot Int Int Underscore Int Int LowerIdent Dot Int Int Underscore ~~~
# PARSE
~~~clojure
(block
  (binop_pipe
    (num_literal_big big:<idx:4>)
    (num_literal_i32 0)
  )
  (num_literal_i32 0)
  (underscore)
  (num_literal_i32 0)
  (num_literal_i32 0)
  (binop_pipe
    (lc "u8")
    (num_literal_i32 0)
  )
  (num_literal_i32 0)
  (underscore)
)
~~~
# FORMATTED
~~~roc
0o0.0 | 0
0_0
_
0
0
u8 | 0
0_
_
~~~
# EXPECTED
LEADING ZERO - :0:0:0:0
MISSING HEADER - fuzz_crash_015.md:1:1:1:4
PARSE ERROR - fuzz_crash_015.md:1:4:1:6
PARSE ERROR - fuzz_crash_015.md:2:1:2:4
PARSE ERROR - fuzz_crash_015.md:3:1:3:4
PARSE ERROR - fuzz_crash_015.md:3:4:3:6
PARSE ERROR - fuzz_crash_015.md:4:1:4:3
# PROBLEMS
**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**fuzz_crash_015.md:2:2:2:3:**
```roc
0_0
```
 ^


**UNDEFINED VARIABLE**
Nothing is named **u8** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_015.md:3:2:3:4:**
```roc
0u8.0
```
 ^^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.

**fuzz_crash_015.md:4:2:4:3:**
```roc
0_
```
 ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_pipe)
  (Expr.num_literal_i32 0)
  (Expr.malformed)
  (Expr.num_literal_i32 0)
  (Expr.num_literal_i32 0)
  (Expr.binop_pipe)
  (Expr.num_literal_i32 0)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 16
(var #0 _)
(var #1 Num *)
(var #2 Num *)
(var #3 _)
(var #4 Num *)
(var #5 _)
(var #6 Num *)
(var #7 Num *)
(var #8 _)
(var #9 Num *)
(var #10 _)
(var #11 Num *)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
~~~
# TYPES
~~~roc
~~~
