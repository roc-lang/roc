# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]s:b->c where module(a).t:c,u:o...
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent TripleDot ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_colon
    (lc "s")
    (binop_colon
      (tuple_literal
        (binop_where
          (binop_arrow_call
            (lc "b")
            (lc "c")
          )
          (binop_colon
            (binop_dot
              (apply_module
                (lc "a")
              )
              (dot_lc "t")
            )
            (lc "c")
          )
        )
        (lc "u")
      )
      (lc "o")
    )
  )
  (ellipsis)
)
~~~
# FORMATTED
~~~roc
module []

s : (b -> c where module(a)..t : c, u) : o
...
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_057.md:1:39:1:42
# PROBLEMS
**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**fuzz_crash_057.md:1:11:1:39:**
```roc
module[]s:b->c where module(a).t:c,u:o...
```
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**fuzz_crash_057.md:1:9:1:10:**
```roc
module[]s:b->c where module(a).t:c,u:o...
```
        ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "s"))
    (type type_15)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 20
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
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
~~~
# TYPES
~~~roc
s : _d
~~~
