# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 b:S
.R
~~~
# TOKENS
~~~text
Int LowerIdent OpColon UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(block
  (num_literal_i32 0)
  (binop_colon
    (lc "b")
    (binop_dot
      (uc "S")
      (uc "R")
    )
  )
)
~~~
# FORMATTED
~~~roc
0
b : S.R
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_018.md:1:1:1:2
PARSE ERROR - fuzz_crash_018.md:2:1:2:3
UNDECLARED TYPE - fuzz_crash_018.md:1:5:1:6
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.num_literal_i32 0)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "b"))
    (type type_5)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 8
(var #0 _)
(var #1 Num *)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
~~~
# TYPES
~~~roc
b : _a
~~~
