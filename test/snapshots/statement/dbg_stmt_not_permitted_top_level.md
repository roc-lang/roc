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
KwModule OpenSquare LowerIdent CloseSquare BlankLine LineComment KwDbg String BlankLine LowerIdent OpAssign TripleDot ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")
))
(block
  (malformed)
  (str_literal_small "foo")
  (binop_equals
    (lc "foo")
    (ellipsis)
  )
)
~~~
# FORMATTED
~~~roc
module [foo]

# not permitted
dbg 
"foo"
foo = ...
~~~
# EXPECTED
INVALID STATEMENT - dbg_stmt_not_permitted_top_level.md:4:1:4:10
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **dbg ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**dbg_stmt_not_permitted_top_level.md:4:1:4:5:**
```roc
dbg "foo"
```
^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.str_literal_small)
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 10
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 Str)
(var #4 -> #9)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
~~~
# TYPES
~~~roc
~~~
