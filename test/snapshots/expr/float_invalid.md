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
TYPE MISMATCH - float_invalid.md:1:1:1:8
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This expression is used in an unexpected way. ─────────────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  3.14.15                                                                   │
 │  ‾‾‾‾‾‾‾                                                                   │
 └────────────────────────────────────────────────────── float_invalid.md:1:1 ┘

    It has the type:

        (_field, _field2, _field3, _field4, _field5, _field6, _field7, _field8,
        _field9, _field10, _field11, _field12, _field13, _field14, _field15,
        _field16)

    But you are trying to use it as:

        Dec

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
NO CHANGE
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
