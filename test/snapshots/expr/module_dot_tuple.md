# META
~~~ini
description=Mod dot malformed (should error)
type=expr
~~~
# SOURCE
~~~roc
I.5
~~~
# EXPECTED
INVALID TUPLE ACCESS - mod_dot_tuple.md:1:1:1:4
# PROBLEMS

┌──────────────────────┐
│ INVALID TUPLE ACCESS ├─ This value is not a tuple, so it has no .5 ─────────┐
└┬─────────────────────┘  element.                                            │
 │                                                                            │
 │  I.5                                                                       │
 │  ‾‾‾                                                                       │
 └─────────────────────────────────────────────────── mod_dot_tuple.md:1:1 ┘


# TOKENS
~~~zig
UpperIdent,NoSpaceDotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple-access
	(e-tag (raw "I"))
	".5")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple-access (index "5")
	(e-tag (name "I")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
