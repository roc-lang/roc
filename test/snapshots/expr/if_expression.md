# META
~~~ini
description=If expression with conditional
type=expr
~~~
# SOURCE
~~~roc
if x > 5 "big" else "small"
~~~
# EXPECTED
UNCONDITIONAL CONDITION - if_expression.md:1:4:1:9
# PROBLEMS

┌─────────────────────────┐
│ UNCONDITIONAL CONDITION ├─ This if condition is known at compile time, so ──┐
└┬────────────────────────┘  this conditional will always make the same       │
 │                           choice.                                          │
 │                                                                            │
 │  if x > 5 "big" else "small"                                               │
 │     ‾‾‾‾‾                                                                  │
 └────────────────────────────────────────────────────── if_expression.md:1:4 ┘


# TOKENS
~~~zig
KwIf,LowerIdent,OpGreaterThan,Int,StringStart,StringPart,StringEnd,KwElse,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-binop (op ">")
		(e-ident (raw "x"))
		(e-int (raw "5")))
	(e-string
		(e-string-part (raw "big")))
	(e-string
		(e-string-part (raw "small"))))
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
			(e-dispatch-call (method "is_gt") (constraint-fn-var 52)
				(receiver
					(e-runtime-error (tag "ident_not_in_scope")))
				(args
					(e-num (value "5"))))
			(e-string
				(e-literal (string "big")))))
	(if-else
		(e-string
			(e-literal (string "small")))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
