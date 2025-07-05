# META
~~~ini
description=crazy_annotation_left2
type=expr
~~~
# SOURCE
~~~roc
1(ts((0
)
#
)
)f:i7f
e
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `ts` in this scope.
Is there an `import` or `exposing` missing up-top?

**TYPE MISMATCH**
This expression is used in an unexpected way:
**crazy_annotation_left2.md:1:1:1:2:**
```roc
1(ts((0
```
^

It is of type:
    _Num(*)_

But you are trying to use it as:
    _* -> *_

# TOKENS
~~~zig
Int(1:1-1:2),NoSpaceOpenRound(1:2-1:3),LowerIdent(1:3-1:5),NoSpaceOpenRound(1:5-1:6),NoSpaceOpenRound(1:6-1:7),Int(1:7-1:8),Newline(1:1-1:1),
CloseRound(2:1-2:2),Newline(1:1-1:1),
Newline(3:2-3:2),
CloseRound(4:1-4:2),Newline(1:1-1:1),
CloseRound(5:1-5:2),LowerIdent(5:2-5:3),OpColon(5:3-5:4),LowerIdent(5:4-5:7),Newline(1:1-1:1),
LowerIdent(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-apply @1.1-5.2
	(e-int @1.1-1.2 (raw "1"))
	(e-apply @1.3-4.2
		(e-ident @1.3-1.5 (raw "ts"))
		(e-tuple @1.6-2.2
			(e-int @1.7-1.8 (raw "0")))))
~~~
# FORMATTED
~~~roc
1(
	ts(
		(
			0,
		),

	),
)
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-5.2
	(e-int @1.1-1.2 (value "1"))
	(e-call @1.3-4.2
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-int @1.7-1.8 (value "0"))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "*"))
~~~
