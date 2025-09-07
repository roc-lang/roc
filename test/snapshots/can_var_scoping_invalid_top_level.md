# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# This should cause an error - var not allowed at top level
var topLevelVar_ = 0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LineComment KwVar LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_equals
    (var_lc "topLevelVar_")
    (num_literal_i32 0)
  )
)
~~~
# FORMATTED
~~~roc
module []

# This should cause an error - var not allowed at top level
var topLevelVar_ = 0
~~~
# EXPECTED
PARSE ERROR - can_var_scoping_invalid_top_level.md:4:1:4:4
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**can_var_scoping_invalid_top_level.md:4:1:4:17:**
```roc
var topLevelVar_ = 0
```
^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.init_var
    (pattern (Patt.var_ident "topLevelVar_"))
    (Expr.num_literal_i32 0)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 _)
(var #2 Num *)
(var #3 _)
(var #4 _)
~~~
# TYPES
~~~roc
~~~
