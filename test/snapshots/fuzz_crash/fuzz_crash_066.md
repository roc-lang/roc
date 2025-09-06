# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module []

C:[0]
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpColon OpenSquare Int CloseSquare ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_colon
    (uc "C")
    (list_literal
      (num_literal_i32 0)
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

C : [0]
~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_066.md:3:4:3:5
MALFORMED TYPE - fuzz_crash_066.md:3:4:3:5
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 6
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
~~~
# TYPES
~~~roc
~~~
