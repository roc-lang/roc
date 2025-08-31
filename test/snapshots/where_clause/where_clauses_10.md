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
~~~
# FORMATTED
~~~roc
module [decode]

import Decode exposing [Decode]

# After member name
decodeThings : # After colon
List List U8 -> List a where # After anno
# after where
module(a) | Decode
~~~
# EXPECTED
NIL
# PROBLEMS
**INVALID WHERE CONSTRAINT**
Invalid where clause constraint syntax.
Where clauses should contain valid ability constraints.

**where_clauses_10.md:9:11:9:14:**
```roc
				module(a).Decode
```
				      ^^^


**INVALID WHERE CONSTRAINT**
Invalid where clause constraint syntax.
Where clauses should contain valid ability constraints.

**where_clauses_10.md:9:14:9:21:**
```roc
				module(a).Decode
```
				         ^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.type_anno
    (name "decodeThings")
    (type binop_where)
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
