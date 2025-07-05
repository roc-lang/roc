# META
~~~ini
description=Multiline list with type mismatch
type=expr
~~~
# SOURCE
~~~roc
[
    42,
    "hello world",
    100
]
~~~
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_multiline_mismatch.md:2:5:2:5
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The first two elements in this list have incompatible types:
**can_list_multiline_mismatch.md:2:5:**
```roc
    42,
    "hello world",
```
    ^^
    ^^^^^^^^^^^^^

The first element has this type:
    _Num(*)_

However, the second element has this type:
    _Str_

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
OpenSquare(1:1-1:2),Newline(1:1-1:1),
Int(2:5-2:7),Comma(2:7-2:8),Newline(1:1-1:1),
StringStart(3:5-3:6),StringPart(3:6-3:17),StringEnd(3:17-3:18),Comma(3:18-3:19),Newline(1:1-1:1),
Int(4:5-4:8),Newline(1:1-1:1),
CloseSquare(5:1-5:2),Newline(1:1-1:1),
MalformedUnknownToken(6:1-6:2),MalformedUnknownToken(6:2-6:3),MalformedUnknownToken(6:3-6:4),EndOfFile(6:4-6:4),
~~~
# PARSE
~~~clojure
(e-list @1.1-5.2
	(e-int @2.5-2.7 (raw "42"))
	(e-string @3.5-3.18
		(e-string-part @3.6-3.17 (raw "hello world")))
	(e-int @4.5-4.8 (raw "100")))
~~~
# FORMATTED
~~~roc
[
	42,
	"hello world",
	100,
]
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-5.2
	(elems
		(e-int @2.5-2.7 (value "42"))
		(e-string @3.5-3.18
			(e-literal @3.6-3.17 (string "hello world")))
		(e-int @4.5-4.8 (value "100"))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "List(Error)"))
~~~
