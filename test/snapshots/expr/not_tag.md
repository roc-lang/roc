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
**TYPE MISMATCH**
This expression is used in an unexpected way:
**not_tag.md:1:3:1:7:**
```roc
!(C(2))
```
  ^^^^

It has the type:
    _[C(Num(_size))]_others_

But I expected it to be:
    _Bool_

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
(e-unary-not
	(e-tag (name "C")
		(args
			(e-num (value "2")))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
