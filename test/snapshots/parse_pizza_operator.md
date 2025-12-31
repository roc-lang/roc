# META
~~~ini
description=Pizza operator (|>) parsing
type=expr
~~~
# SOURCE
~~~roc
1 |> add 2 |> mul 3
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int,OpPizza,LowerIdent,Int,OpPizza,LowerIdent,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "|>")
	(e-binop (op "|>")
		(e-int (raw "1"))
		(e-apply
			(e-ident (raw "add"))
			(e-int (raw "2"))))
	(e-apply
		(e-ident (raw "mul"))
		(e-int (raw "3"))))
~~~
