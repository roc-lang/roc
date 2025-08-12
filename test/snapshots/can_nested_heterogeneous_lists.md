# META
~~~ini
description=Nested heterogeneous lists
type=expr
~~~
# SOURCE
~~~roc
[[1, "hello"], [2, 3]]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_nested_heterogeneous_lists.md:1:3:1:3
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The two elements in this list have incompatible types:
**can_nested_heterogeneous_lists.md:1:3:**
```roc
[[1, "hello"], [2, 3]]
```
  ^  ^^^^^^^

The first element has this type:
    _Num(_size)_

However, the second element has this type:
    _Str_

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
OpenSquare(1:1-1:2),OpenSquare(1:2-1:3),Int(1:3-1:4),Comma(1:4-1:5),StringStart(1:6-1:7),StringPart(1:7-1:12),StringEnd(1:12-1:13),CloseSquare(1:13-1:14),Comma(1:14-1:15),OpenSquare(1:16-1:17),Int(1:17-1:18),Comma(1:18-1:19),Int(1:20-1:21),CloseSquare(1:21-1:22),CloseSquare(1:22-1:23),EndOfFile(1:23-1:23),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.23
	(e-list @1.2-1.14
		(e-int @1.3-1.4 (raw "1"))
		(e-string @1.6-1.13
			(e-string-part @1.7-1.12 (raw "hello"))))
	(e-list @1.16-1.22
		(e-int @1.17-1.18 (raw "2"))
		(e-int @1.20-1.21 (raw "3"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-1.23
	(elems
		(e-list @1.2-1.14
			(elems
				(e-int @1.3-1.4 (value "1"))
				(e-string @1.6-1.13
					(e-literal @1.7-1.12 (string "hello")))))
		(e-list @1.16-1.22
			(elems
				(e-int @1.17-1.18 (value "2"))
				(e-int @1.20-1.21 (value "3"))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.23 (type "List(List(Error))"))
~~~
