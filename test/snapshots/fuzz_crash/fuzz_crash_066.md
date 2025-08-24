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
KwModule OpenSquare CloseSquare UpperIdent OpColon OpenSquare Int CloseSquare ~~~
# PARSE
~~~clojure
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
NO CHANGE
~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_066.md:3:4:3:5
MALFORMED TYPE - fuzz_crash_066.md:3:4:3:5
# PROBLEMS
**Pattern in Expression Context**
at 3:1 to 3:2

**Unsupported Node**
at 3:3 to 3:5

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
