# META
~~~ini
description=Example if-then-else statement with a tag expression
type=expr
~~~
# SOURCE
~~~roc
if Bool.True Ok(0) else Err(1)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwIf(1:1-1:3),UpperIdent(1:4-1:8),NoSpaceDotUpperIdent(1:8-1:13),UpperIdent(1:14-1:16),NoSpaceOpenRound(1:16-1:17),Int(1:17-1:18),CloseRound(1:18-1:19),KwElse(1:20-1:24),UpperIdent(1:25-1:28),NoSpaceOpenRound(1:28-1:29),Int(1:29-1:30),CloseRound(1:30-1:31),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-1.31
	(e-tag @1.4-1.13 (raw "Bool.True"))
	(e-apply @1.14-1.19
		(e-tag @1.14-1.16 (raw "Ok"))
		(e-int @1.17-1.18 (raw "0")))
	(e-apply @1.25-1.31
		(e-tag @1.25-1.28 (raw "Err"))
		(e-int @1.29-1.30 (raw "1"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-1.31
	(if-branches
		(if-branch
			(e-nominal @1.4-1.13 (nominal "Bool")
				(e-tag @1.4-1.13 (name "True")))
			(e-nominal @1.14-1.19 (nominal "Result")
				(e-tag @1.14-1.19 (name "Ok")
					(args
						(e-int @1.17-1.18 (value "0")))))))
	(if-else
		(e-nominal @1.25-1.31 (nominal "Result")
			(e-tag @1.25-1.31 (name "Err")
				(args
					(e-int @1.29-1.30 (value "1")))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.31 (type "Result(Num(_size), Num(_size2))"))
~~~
