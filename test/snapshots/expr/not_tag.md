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
**MISSING METHOD**
This **not** method is being called on a value whose type doesn't have that method:
**not_tag.md:1:1:1:8:**
```roc
!(C(2))
```
^^^^^^^

The value's type, which does not have a method named**not**, is:

    [C(a), .._others]
      where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]

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
