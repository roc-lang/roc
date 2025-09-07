# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import	B	as
G	if 0{}else||0
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly KwImport UpperIdent KwAs UpperIdent KwIf Int OpenCurly CloseCurly KwElse OpOr Int ~~~
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
  (import
    (binop_as
      (uc "B")
      (uc "G")
    )
  )
  (if_else
    (condition       (num_literal_i32 0)
)
    (then       (record_literal)
)
    (else       (malformed)
))
  (num_literal_i32 0)
)
~~~
# FORMATTED
~~~roc
app { f: "" platform [] }

import B as G
if 0 {} else ||
0
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_059.md:2:3:2:5
PARSE ERROR - fuzz_crash_059.md:2:6:2:7
PARSE ERROR - fuzz_crash_059.md:2:7:2:8
PARSE ERROR - fuzz_crash_059.md:2:8:2:9
PARSE ERROR - fuzz_crash_059.md:2:9:2:13
PARSE ERROR - fuzz_crash_059.md:2:13:2:14
PARSE ERROR - fuzz_crash_059.md:2:14:2:15
PARSE ERROR - fuzz_crash_059.md:2:15:2:16
MODULE NOT FOUND - fuzz_crash_059.md:1:20:2:2
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **||** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_059.md:2:13:2:15:**
```roc
G	if 0{}else||0
```
 	          ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Expr.if_else)
  (Expr.num_literal_i32 0)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 19
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
(var #10 -> #16)
(var #11 -> #18)
(var #12 _)
(var #13 -> #18)
(var #14 Num *)
(var #15 _)
(var #16 Num *)
(var #17 -> #18)
(var #18 {})
~~~
# TYPES
~~~roc
~~~
