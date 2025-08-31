# META
~~~ini
description=where_clauses (7)
type=file
~~~
# SOURCE
~~~roc
module [Hash]

Hash(a, hasher) # After header
	: # After colon
		a # After var
			where # After where
				module(a).hash : hasher # After method
					-> # After arrow
						hasher, # After first clause
				module(hasher).Hasher

Decode(a) : a
	where
		module(a).decode( # After method args open
			List(U8), # After method arg
		) -> a
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LineComment OpColon LineComment LowerIdent LineComment KwWhere LineComment KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent LineComment OpArrow LineComment LowerIdent Comma LineComment KwModule OpenRound LowerIdent CloseRound Dot UpperIdent BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpenRound LineComment UpperIdent OpenRound UpperIdent CloseRound Comma LineComment CloseRound OpArrow LowerIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Hash")
))
~~~
# FORMATTED
~~~roc
module [Hash]

Hash((a, hasher)) : 
	(a where module(a).hash : hasher) -> hasher,
	module(hasher) | Hasher,
Decode(a) : a where module(a).decode(List(U8)) -> a# After header
# After colon
# After var
# After where
# After method
# After arrow
# After first clause
# After method args open
# After method arg
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_7.md:7:11:7:14:**
```roc
				module(a).hash : hasher # After method
```
				      ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_7.md:10:11:10:19:**
```roc
				module(hasher).Hasher
```
				      ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**where_clauses_7.md:14:9:14:12:**
```roc
		module(a).decode( # After method args open
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
        (Expr.apply_ident)
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
