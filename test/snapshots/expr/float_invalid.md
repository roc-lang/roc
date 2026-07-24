# META
~~~ini
description=Invalid float literal too many decimal points
type=expr
~~~
# SOURCE
~~~roc
3.14.15
~~~
# EXPECTED
INVALID TUPLE ACCESS - float_invalid.md:1:1:1:8
# PROBLEMS

┌──────────────────────┐
│ INVALID TUPLE ACCESS ├─ This value is not a tuple, so it has no .15 ────────┐
└┬─────────────────────┘  element.                                            │
 │                                                                            │
 │  3.14.15                                                                   │
 │  ‾‾‾‾‾‾‾                                                                   │
 └────────────────────────────────────────────────────── float_invalid.md:1:1 ┘


# TOKENS
~~~zig
Float,NoSpaceDotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple-access
	(e-frac (raw "3.14"))
	".15")
~~~
# FORMATTED
~~~roc
(3.14).15
~~~
# CANONICALIZE
~~~clojure
(e-tuple-access (index "15")
	(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
