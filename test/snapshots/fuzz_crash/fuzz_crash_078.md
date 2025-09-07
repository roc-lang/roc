# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]e="""
import#\
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign MultilineString KwImport LineComment ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_equals
    (lc "e")
    (str_literal_small "")
  )
)
~~~
# FORMATTED
~~~roc
module []

e = """
import

#\
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_078.md:3:1:3:1
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_078.md:1:9:1:10:**
```roc
module[]e="""
```
        ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "e"))
    (Expr.str_literal_small)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 -> #2)
(var #2 Str)
(var #3 _)
(var #4 _)
~~~
# TYPES
~~~roc
e : Str
~~~
