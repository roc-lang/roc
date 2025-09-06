# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]import
S
0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport UpperIdent Int ~~~
# PARSE
~~~clojure
(module-header)
(block
  (import
    (uc "S")
  )
  (num_literal_i32 0)
)
~~~
# FORMATTED
~~~roc
module []

import S
0
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_052.md:3:1:3:2
MODULE NOT FOUND - fuzz_crash_052.md:1:9:2:2
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Expr.num_literal_i32 0)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 Num *)
(var #4 _)
~~~
# TYPES
~~~roc
~~~
