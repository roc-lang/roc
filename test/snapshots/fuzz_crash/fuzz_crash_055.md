# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]r:a	where
module(a).h:s
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_colon
    (lc "r")
    (binop_where
      (lc "a")
      (binop_colon
        (binop_dot
          (apply_module
            (lc "a")
          )
          (dot_lc "h")
        )
        (lc "s")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

r : a where module(a)..h : s
~~~
# EXPECTED
NIL
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_055.md:1:9:1:10:**
```roc
module[]r:a	where
```
        ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "r"))
    (type type_9)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 12
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
(var #10 _)
(var #11 _)
~~~
# TYPES
~~~roc
r : _b
~~~
