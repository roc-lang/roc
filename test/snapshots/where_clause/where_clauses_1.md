# META
~~~ini
description=where_clauses (1)
type=file
~~~
# SOURCE
~~~roc
module [Hash, Decode]

Hash(a, hasher) : a
	where
		module(a).hash : hasher -> hasher,
		module(hasher).Hasher

Decode(a) : a where module(a).decode : List(U8) -> a
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma UpperIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent Comma KwModule OpenRound LowerIdent CloseRound Dot UpperIdent BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow LowerIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Hash")

    (uc "Decode")
))
~~~
# FORMATTED
~~~roc
module [Hash, Decode]

Hash((a, hasher)) : (a where module(a).hash : hasher) -> hasher, module(hasher) | Hasher
Decode(a) : a where module(a).decode : List U8 -> a
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_1.md:5:9:5:12:**
```roc
		module(a).hash : hasher -> hasher,
```
		      ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_1.md:6:9:6:17:**
```roc
		module(hasher).Hasher
```
		      ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_1.md:8:27:8:30:**
```roc
Decode(a) : a where module(a).decode : List(U8) -> a
```
                          ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.tuple_literal
      (Expr.binop_thin_arrow
        (Expr.binop_colon
          (Expr.lookup "a")
          (Expr.binop_colon
            (Expr.lambda)
            (Expr.lookup "hasher")
          )
        )
        (Expr.lookup "hasher")
      )
      (Expr.lambda)
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_thin_arrow
      (Expr.binop_colon
        (Expr.lookup "a")
        (Expr.binop_colon
          (Expr.lambda)
          (Expr.apply_tag)
        )
      )
      (Expr.lookup "a")
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
