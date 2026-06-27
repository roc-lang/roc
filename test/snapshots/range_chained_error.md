# META
~~~ini
description=Chained range operators are rejected
type=snippet
~~~
# SOURCE
~~~roc
r = 1..<5..<10
~~~
# EXPECTED
CHAINED RANGE - range_chained_error.md:1:5:1:15
# PROBLEMS

┌───────────────┐
│ CHAINED RANGE ├─ Range operators can't be chained. Write a single range ────┐
└┬──────────────┘  instead, like `a..<b` or `a..=b`.                          │
 │                                                                            │
 │  r = 1..<5..<10                                                            │
 │      ‾‾‾‾‾‾‾‾‾‾                                                            │
 └──────────────────────────────────────────────── range_chained_error.md:1:5 ┘


# TOKENS
~~~zig
LowerIdent,OpAssign,Int,OpDoubleDotLessThan,Int,OpDoubleDotLessThan,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "r"))
			(e-binop (op "..<")
				(e-binop (op "..<")
					(e-int (raw "1"))
					(e-int (raw "5")))
				(e-int (raw "10"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-runtime-error (tag "range_op_chained"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
