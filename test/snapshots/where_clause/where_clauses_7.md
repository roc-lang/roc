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
(block
  (binop_colon
    (apply_uc
      (uc "Hash")
      (tuple_literal
        (lc "a")
        (lc "hasher")
      )
    )
    (tuple_literal
      (binop_arrow_call
        (binop_where
          (lc "a")
          (binop_colon
            (binop_dot
              (apply_module
                (lc "a")
              )
              (dot_lc "hash")
            )
            (lc "hasher")
          )
        )
        (lc "hasher")
      )
      (binop_dot
        (apply_module
          (lc "hasher")
        )
        (uc "Hasher")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "Decode")
      (lc "a")
    )
    (binop_arrow_call
      (binop_where
        (lc "a")
        (apply_anon
          (binop_dot
            (apply_module
              (lc "a")
            )
            (dot_lc "decode")
          )
          (apply_uc
            (uc "List")
            (uc "U8")
          )
        )
      )
      (lc "a")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [Hash]

# After header
Hash((a, hasher)) : # After colon
	(a where # After var
	# After where
module(a)..hash : hasher) -> # After method
	# After arrow
hasher, # After first clause
	module(hasher).Hasher,
Decode(a) : a where module(a)..decode( # After method args open
List(U8)) -> # After method arg
a
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_7.md:3:1:10:26
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_7.md:12:1:16:9
# PROBLEMS
**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**where_clauses_7.md:10:11:10:19:**
```roc
				module(hasher).Hasher
```
				      ^^^^^^^^


**INVALID WHERE CONSTRAINT**
Invalid where clause constraint syntax.
Where clauses should contain valid ability constraints.

**where_clauses_7.md:14:9:16:4:**
```roc
		module(a).decode( # After method args open
			List(U8), # After method arg
		) -> a
```


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_alias)
  (Stmt.type_alias)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 40
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
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
~~~
# TYPES
~~~roc
~~~
