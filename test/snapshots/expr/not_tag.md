# META
~~~ini
description=not_tag
type=expr
~~~
# SOURCE
~~~roc
!(C(2))
~~~
# EXPECTED
MISSING METHOD - not_tag.md:1:1:1:8
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ This `not` method is being called on a value whose type ──┐
└┬───────────────┘  doesn't have that method.                                 │
 │                                                                            │
 │  !(C(2))                                                                   │
 │  ‾‾‾‾‾‾‾                                                                   │
 └──────────────────────────────────────────────────────────── not_tag.md:1:1 ┘

    The value's type, which does not have a method named `not`, is:

        [C(a), ..] where [a.from_numeral : Numeral -> Try(a,
        [InvalidNumeral(Str)])]

# TOKENS
~~~zig
OpBang,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(unary "!"
	(e-tuple
		(e-apply
			(e-tag (raw "C"))
			(e-int (raw "2")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dispatch-call (method "not") (constraint-fn-var 43)
	(receiver
		(e-tag (name "C")
			(args
				(e-num (value "2")))))
	(args))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
