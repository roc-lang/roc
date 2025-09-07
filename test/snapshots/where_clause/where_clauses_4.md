# META
~~~ini
description=where_clauses (4)
type=file
~~~
# SOURCE
~~~roc
module [decodeThings]

import Decode exposing [Decode]

decodeThings : List(List(U8)) -> List(a)
	where module(a).Decode
decodeThings = ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport UpperIdent KwExposing OpenSquare UpperIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent CloseRound CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound KwWhere KwModule OpenRound LowerIdent CloseRound Dot UpperIdent LowerIdent OpAssign TripleDot ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "decodeThings")
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
  (binop_equals
    (lc "decodeThings")
    (ellipsis)
  )
)
~~~
# FORMATTED
~~~roc
module [decodeThings]

import Decode exposing [Decode]
decodeThings : List List U8 -> List a where module(a).Decode
decodeThings = ...
~~~
# EXPECTED
MODULE NOT FOUND - where_clauses_4.md:3:1:3:32
# PROBLEMS
**INVALID WHERE CONSTRAINT**
Invalid where clause constraint syntax.
Where clauses should contain valid ability constraints.

**where_clauses_4.md:6:14:6:24:**
```roc
	where module(a).Decode
```
	            ^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**where_clauses_4.md:7:1:7:13:**
```roc
decodeThings = ...
```
^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "decodeThings"))
    (type type_21)
  )
  (Stmt.assign
    (pattern (Patt.ident "decodeThings"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 28
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
(var #23 -> #27)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
~~~
# TYPES
~~~roc
decodeThings : _b
~~~
