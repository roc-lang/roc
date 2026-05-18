# META
~~~ini
description=if_then_else (5)
type=expr
~~~
# SOURCE
~~~roc
if bool { # Comment after then open
	A # Comment after expr
} else B
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwIf,LowerIdent,OpenCurly,
UpperIdent,
CloseCurly,KwElse,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-ident (raw "bool"))
	(e-block
		(statements
			(e-tag (raw "A"))))
	(e-tag (raw "B")))
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
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-block
				(e-tag (name "A")))))
	(if-else
		(e-tag (name "B"))))
~~~
# TYPES
~~~clojure
(expr (type "[A, B, ..]"))
~~~
