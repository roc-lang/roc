# META
~~~ini
description=triple_paren_pat_ann
type=expr
~~~
# SOURCE
~~~roc
1((0(0
)))f:f
i
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**triple_paren_pat_ann.md:1:4:1:5:**
```roc
1((0(0
```
   ^

It is of type:
    _Num(*)_

But you are trying to use it as:
    _Num(*) -> *_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**triple_paren_pat_ann.md:1:1:1:2:**
```roc
1((0(0
```
^

It is of type:
    _Num(*)_

But you are trying to use it as:
    _* -> *_

# TOKENS
~~~zig
Int(1:1-1:2),NoSpaceOpenRound(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Int(1:4-1:5),NoSpaceOpenRound(1:5-1:6),Int(1:6-1:7),Newline(1:1-1:1),
CloseRound(2:1-2:2),CloseRound(2:2-2:3),CloseRound(2:3-2:4),LowerIdent(2:4-2:5),OpColon(2:5-2:6),LowerIdent(2:6-2:7),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-apply @1.1-2.4
	(e-int @1.1-1.2 (raw "1"))
	(e-tuple @1.3-2.3
		(e-apply @1.4-2.2
			(e-int @1.4-1.5 (raw "0"))
			(e-int @1.6-1.7 (raw "0")))))
~~~
# FORMATTED
~~~roc
1(
	(
		0(
			0,
		),
	),
)
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-2.4
	(e-int @1.1-1.2 (value "1"))
	(e-call @1.4-2.2
		(e-int @1.4-1.5 (value "0"))
		(e-int @1.6-1.7 (value "0"))))
~~~
# TYPES
~~~clojure
(expr @1.1-2.4 (type "*"))
~~~
