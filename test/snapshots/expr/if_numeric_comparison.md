# META
~~~ini
description=If expression with numeric comparison
type=expr
~~~
# SOURCE
~~~roc
if 5 > 3 1 else 2
~~~
# EXPECTED
UNCONDITIONAL CONDITION - if_numeric_comparison.md:1:4:1:9
# PROBLEMS

┌─────────────────────────┐
│ UNCONDITIONAL CONDITION ├─ This if condition is known at compile time, so ──┐
└┬────────────────────────┘  this conditional will always make the same       │
 │                           choice.                                          │
 │                                                                            │
 │  if 5 > 3 1 else 2                                                         │
 │     ‾‾‾‾‾                                                                  │
 └────────────────────────────────────────────── if_numeric_comparison.md:1:4 ┘


# TOKENS
~~~zig
KwIf,Int,OpGreaterThan,Int,Int,KwElse,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-binop (op ">")
		(e-int (raw "5"))
		(e-int (raw "3")))
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
			(e-dispatch-call (method "is_gt") (constraint-fn-var 82)
				(receiver
					(e-num (value "5")))
				(args
					(e-num (value "3"))))
			(e-num (value "1"))))
	(if-else
		(e-num (value "2"))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
