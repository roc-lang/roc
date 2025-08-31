# META
~~~ini
description=Module dispatch in where clause
type=file
~~~
# SOURCE
~~~roc
module [deserialize]

deserialize : List(U8) -> Result(a, [DecodeErr])
	where module(a).decode : List(U8) -> Result(a, [DecodeErr])
deserialize = |_| ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma OpenSquare UpperIdent CloseSquare CloseRound KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma OpenSquare UpperIdent CloseSquare CloseRound LowerIdent OpAssign OpBar Underscore OpBar TripleDot ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "deserialize")
))
~~~
# FORMATTED
~~~roc
module [deserialize]

deserialize : List U8 -> Result(a, [DecodeErr]) where module(a).decode : List U8 -> Result(a, [DecodeErr])
deserialize = |_| ...
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_serde_example.md:4:14:4:17:**
```roc
	where module(a).decode : List(U8) -> Result(a, [DecodeErr])
```
	            ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "deserialize")
    (Expr.binop_thin_arrow
      (Expr.binop_colon
        (Expr.binop_thin_arrow
          (Expr.apply_tag)
          (Expr.apply_tag)
        )
        (Expr.binop_colon
          (Expr.lambda)
          (Expr.apply_tag)
        )
      )
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "deserialize")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
deserialize : _b
~~~
