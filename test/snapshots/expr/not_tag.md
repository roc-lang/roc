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
TYPE MISMATCH - not_tag.md:1:3:1:7
# PROBLEMS
── ✗ type mismatch ────────────────────────────────────────────── not_tag.md:1:3

The first argument being passed to this function has the wrong type.

!(C(2))
  ^^^^

This argument has the type:

    [C(a), ..] where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]

But the function needs the first argument to be:

    Bool

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
(e-call
	(e-lookup-associated-resolved (source "Bool.not") (builtin) (target-node "17379") (target-def "17379"))
	(e-tag (name "C")
		(args
			(e-num (value "2")))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
