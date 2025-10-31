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
KwIf,UpperIdent,NoSpaceDotUpperIdent,UpperIdent,NoSpaceOpenRound,Int,CloseRound,KwElse,UpperIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-tag (raw "Bool.True"))
	(e-apply
		(e-tag (raw "Ok"))
		(e-int (raw "0")))
	(e-apply
		(e-tag (raw "Err"))
		(e-int (raw "1"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if
	(if-branches
		(if-branch
			(e-nominal-external
				(builtin)
				(e-tag (name "True")))
			(e-tag (name "Ok")
				(args
					(e-num (value "0"))))))
	(if-else
		(e-tag (name "Err")
			(args
				(e-num (value "1"))))))
~~~
# TYPES
~~~clojure
(expr (type "[Ok(Num(_size)), Err(Num(_size2))]_others"))
~~~
