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
TYPE MISMATCH - not_tag.md:1:1:1:8
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**not_tag.md:1:1:1:8:**
```roc
!(C(2))
```
^^^^^^^

It is of type:
    _Bool_

But you are trying to use it as:
    _[C(Num(_size))]_others_

# TOKENS
~~~zig
OpBang(1:1-1:2),NoSpaceOpenRound(1:2-1:3),UpperIdent(1:3-1:4),NoSpaceOpenRound(1:4-1:5),Int(1:5-1:6),CloseRound(1:6-1:7),CloseRound(1:7-1:8),EndOfFile(1:8-1:8),
~~~
# PARSE
~~~clojure
(unary "!"
	(e-tuple @1.2-1.8
		(e-apply @1.3-1.7
			(e-tag @1.3-1.4 (raw "C"))
			(e-int @1.5-1.6 (raw "2")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-unary-not @1.1-1.8
	(e-tag @1.3-1.7 (name "C")
		(args
			(e-int @1.5-1.6 (value "2")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.8 (type "Error"))
~~~
