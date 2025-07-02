# META
~~~ini
description=crazy_annotation_left
type=expr
~~~
# SOURCE
~~~roc
1((0(#
0)
))f:f
t
~~~
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**crazy_annotation_left.md:1:4:1:5:**
```roc
1((0(#
```
   ^

It is of type:
    _Num(*)_

But you are trying to use it as:
    _Num(*) -> *_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**crazy_annotation_left.md:1:1:1:2:**
```roc
1((0(#
```
^

It is of type:
    _Num(*)_

But you are trying to use it as:
    _(*) -> *_

# TOKENS
~~~zig
Int(1:1-1:2),NoSpaceOpenRound(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Int(1:4-1:5),NoSpaceOpenRound(1:5-1:6),Newline(1:7-1:7),
Int(2:1-2:2),CloseRound(2:2-2:3),Newline(1:1-1:1),
CloseRound(3:1-3:2),CloseRound(3:2-3:3),LowerIdent(3:3-3:4),OpColon(3:4-3:5),LowerIdent(3:5-3:6),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-apply @1.1-3.3
	(e-int @1.1-1.2 (raw "1"))
	(e-tuple @1.3-3.2
		(e-apply @1.4-2.3
			(e-int @1.4-1.5 (raw "0"))
			(e-int @2.1-2.2 (raw "0")))))
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
(e-call @1.1-3.3
	(e-int @1.1-1.2 (value "1"))
	(e-tuple @1.3-3.2
		(elems
			(e-call @1.4-2.3
				(e-int @1.4-1.5 (value "0"))
				(e-int @2.1-2.2 (value "0"))))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.3 (type "*"))
~~~
