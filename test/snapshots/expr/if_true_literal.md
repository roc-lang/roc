# META
~~~ini
description=If expression with True boolean literal
type=expr
~~~
# SOURCE
~~~roc
if True 1 else 2
~~~
# EXPECTED
UNCONDITIONAL CONDITION - if_true_literal.md:1:4:1:8
# PROBLEMS

┌─────────────────────────┐
│ UNCONDITIONAL CONDITION ├─ This if condition is known at compile time, so ──┐
└┬────────────────────────┘  this conditional will always make the same       │
 │                           choice.                                          │
 │                                                                            │
 │  if True 1 else 2                                                          │
 │     ‾‾‾‾                                                                   │
 └──────────────────────────────────────────────────── if_true_literal.md:1:4 ┘


# TOKENS
~~~zig
KwIf,UpperIdent,Int,KwElse,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-tag (raw "True"))
	(e-int (raw "1"))
	(e-int (raw "2")))
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
			(e-tag (name "True"))
			(e-num (value "1"))))
	(if-else
		(e-num (value "2"))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
