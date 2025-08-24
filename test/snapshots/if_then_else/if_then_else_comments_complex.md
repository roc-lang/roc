# META
~~~ini
description=if_then_else (15)
type=expr
~~~
# SOURCE
~~~roc
if # Comment after if
	bool # Comment after cond
		{ # Comment after then open
			1
		} # Comment after then close
			else # Comment after else
				{ # Comment else open
					2
				}
~~~
# TOKENS
~~~text
KwIf LowerIdent OpenCurly Int CloseCurly KwElse OpenCurly Int CloseCurly ~~~
# PARSE
~~~clojure
(if_else <0 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_comments_complex.md:2:2:2:6
# PROBLEMS
**Parse Error**
at 1:1 to 3:3

**Unsupported Node**
at 1:1 to 9:5

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
