# META
~~~ini
description=pnc_parens_apply_etc
type=expr
~~~
# SOURCE
~~~roc
(
3)():B
(z)
~~~
# EXPECTED
TYPE MISMATCH - pnc_parens_apply_etc.md:2:1:2:2
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**pnc_parens_apply_etc.md:2:1:2:2:**
```roc
3)():B
```
^

It is of type:
    _Num(*)_

But you are trying to use it as:
    _({}) -> *_

# TOKENS
~~~zig
OpenRound(1:1-1:2),Newline(1:1-1:1),
Int(2:1-2:2),CloseRound(2:2-2:3),NoSpaceOpenRound(2:3-2:4),CloseRound(2:4-2:5),OpColon(2:5-2:6),UpperIdent(2:6-2:7),Newline(1:1-1:1),
OpenRound(3:1-3:2),LowerIdent(3:2-3:3),CloseRound(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-apply @1.1-2.5
	(e-tuple @1.1-2.3
		(e-int @2.1-2.2 (raw "3"))))
~~~
# FORMATTED
~~~roc
(
	3,
)()
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-2.5
	(e-int @2.1-2.2 (value "3")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.5 (type "*"))
~~~
