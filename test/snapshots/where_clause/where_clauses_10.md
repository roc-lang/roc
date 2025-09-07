# META
~~~ini
description=where_clauses (10)
type=file
~~~
# SOURCE
~~~roc
module [decode]

import Decode exposing [Decode]

decodeThings # After member name
	: # After colon
		List(List(U8)) -> List(a) # After anno
			where # after where
				module(a).Decode
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport UpperIdent KwExposing OpenSquare UpperIdent CloseSquare BlankLine LowerIdent LineComment OpColon LineComment UpperIdent OpenRound UpperIdent OpenRound UpperIdent CloseRound CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound LineComment KwWhere LineComment KwModule OpenRound LowerIdent CloseRound Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "decode")
))
(block
  (import
    (binop_exposing
      (uc "Decode")
      (list_literal
        (uc "Decode")
      )
    )
  )
  (binop_colon
    (lc "decodeThings")
    (binop_where
      (binop_arrow_call
        (apply_uc
          (uc "List")
          (apply_uc
            (uc "List")
            (uc "U8")
          )
        )
        (apply_uc
          (uc "List")
          (lc "a")
        )
      )
      (binop_dot
        (apply_module
          (lc "a")
        )
        (uc "Decode")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [decode]

import Decode exposing [Decode]
# After member name
decodeThings : # After colon
List List U8 -> List a where # After anno
# after where
module(a).Decode
~~~
# EXPECTED
MODULE NOT FOUND - where_clauses_10.md:3:1:3:32
EXPOSED BUT NOT DEFINED - where_clauses_10.md:1:9:1:15
# PROBLEMS
**INVALID WHERE CONSTRAINT**
Invalid where clause constraint syntax.
Where clauses should contain valid ability constraints.

**where_clauses_10.md:9:11:9:21:**
```roc
				module(a).Decode
```
				      ^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**where_clauses_10.md:5:1:5:13:**
```roc
decodeThings # After member name
```
^^^^^^^^^^^^


**EXPOSED BUT NOT IMPLEMENTED**
This value is exposed in the module header but not defined in the module.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "decodeThings"))
    (type type_21)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 24
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
~~~
# TYPES
~~~roc
decodeThings : _b
~~~
