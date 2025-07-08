# META
~~~ini
description=Heterogeneous nested list causes type mismatch
type=expr
~~~
# SOURCE
~~~roc
[[], [1], ["hello"]]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_nested_heterogeneous.md:1:6:1:6
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The second and third elements in this list have incompatible types:
**can_list_nested_heterogeneous.md:1:6:**
```roc
[[], [1], ["hello"]]
```
     ^^^  ^^^^^^^^^

The second element has this type:
    _List(Num(*))_

However, the third element has this type:
    _List(Str)_

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
OpenSquare(1:1-1:2),OpenSquare(1:2-1:3),CloseSquare(1:3-1:4),Comma(1:4-1:5),OpenSquare(1:6-1:7),Int(1:7-1:8),CloseSquare(1:8-1:9),Comma(1:9-1:10),OpenSquare(1:11-1:12),StringStart(1:12-1:13),StringPart(1:13-1:18),StringEnd(1:18-1:19),CloseSquare(1:19-1:20),CloseSquare(1:20-1:21),EndOfFile(1:21-1:21),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.21
	(e-list @1.2-1.4)
	(e-list @1.6-1.9
		(e-int @1.7-1.8 (raw "1")))
	(e-list @1.11-1.20
		(e-string @1.12-1.19
			(e-string-part @1.13-1.18 (raw "hello")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-1.21
	(elems
		(e-empty-list @1.2-1.4)
		(e-list @1.6-1.9
			(elems
				(e-int @1.7-1.8 (value "1"))))
		(e-list @1.11-1.20
			(elems
				(e-str @1.12-1.19
					(e-literal @1.13-1.18 (string "hello")))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.21 (type "List(Error)"))
~~~
