# META
~~~ini
description=comment_indent_in_parens
type=expr
~~~
# SOURCE
~~~roc
1((0#
)#
):gi
M
~~~
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**comment_indent_in_parens.md:1:1:1:2:**
```roc
1((0#
```
^

It is of type:
    _Num(b)_

But you are trying to use it as:
    _(Num(a)) -> a_

# TOKENS
~~~zig
Int(1:1-1:2),NoSpaceOpenRound(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Int(1:4-1:5),Newline(1:6-1:6),
CloseRound(2:1-2:2),Newline(2:3-2:3),
CloseRound(3:1-3:2),OpColon(3:2-3:3),LowerIdent(3:3-3:5),Newline(1:1-1:1),
UpperIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-apply @1.1-3.2
	(e-int @1.1-1.2 (raw "1"))
	(e-tuple @1.3-2.2
		(e-int @1.4-1.5 (raw "0"))))
~~~
# FORMATTED
~~~roc
1(
	(
		0,
	),
)
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-3.2
	(e-int @1.1-1.2 (value "1"))
	(e-tuple @1.3-2.2
		(elems
			(e-int @1.4-1.5 (value "0")))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "a"))
~~~
