# META
~~~ini
description=Where clause with multiline constraints
type=file
~~~
# SOURCE
~~~roc
module [process]

process : a, b -> c
	where
		module(a).convert : a -> c,
		module(b).transform : b -> c
process = ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign TripleDot ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "process")
))
~~~
# FORMATTED
~~~roc
module [process]

process : a -> b -> (c where module(a).convert : a) -> c, module(b).transform : b -> c
process = ...
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_multiline.md:5:9:5:12:**
```roc
		module(a).convert : a -> c,
```
		      ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_multiline.md:6:9:6:12:**
```roc
		module(b).transform : b -> c
```
		      ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "process")
    (Expr.binop_thin_arrow
      (Expr.lookup "a")
      (Expr.binop_thin_arrow
        (Expr.lookup "b")
        (Expr.binop_thin_arrow
          (Expr.binop_colon
            (Expr.tuple_literal
              (Expr.binop_thin_arrow
                (Expr.binop_colon
                  (Expr.lookup "c")
                  (Expr.binop_colon
                    (Expr.lambda)
                    (Expr.lookup "a")
                  )
                )
                (Expr.lookup "c")
              )
              (Expr.lambda)
            )
            (Expr.lookup "b")
          )
          (Expr.lookup "c")
        )
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "process")
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
process : Error
~~~
