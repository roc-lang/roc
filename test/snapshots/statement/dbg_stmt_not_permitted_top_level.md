# META
~~~ini
description=Debug expression not permitted at the top level
type=file
~~~
# SOURCE
~~~roc
module [foo]

# not permitted
dbg "foo"

foo = ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare KwDbg String LowerIdent OpAssign TripleDot ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
  (str_literal_small "foo")
  (binop_equals
    (lc "foo")
    (ellipsis)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
INVALID STATEMENT - dbg_stmt_not_permitted_top_level.md:4:1:4:10
# PROBLEMS
**Parse Error**
at 4:1 to 4:1

**Unsupported Node**
at 4:1 to 4:1

**Unsupported Node**
at 6:7 to 6:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.str_literal_small)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
