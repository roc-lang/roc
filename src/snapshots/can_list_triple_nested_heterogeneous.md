# META
~~~ini
description=Triply-nested heterogeneous list causes type mismatch
type=expr
~~~
# SOURCE
~~~roc
[[], [[], [1]], [[], ["hello"]]]
~~~
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The second and third elements in this list have incompatible types:
**can_list_triple_nested_heterogeneous.md:1:6:**
```roc
[[], [[], [1]], [[], ["hello"]]]
```
     ^^^^^^^^^  ^^^^^^^^^^^^^^^

The second element has this type:
    _List(List(Num(*)))_

However, the third element has this type:
    _List(List(Str))_

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
OpenSquare(1:1-1:2),OpenSquare(1:2-1:3),CloseSquare(1:3-1:4),Comma(1:4-1:5),OpenSquare(1:6-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),Comma(1:9-1:10),OpenSquare(1:11-1:12),Int(1:12-1:13),CloseSquare(1:13-1:14),CloseSquare(1:14-1:15),Comma(1:15-1:16),OpenSquare(1:17-1:18),OpenSquare(1:18-1:19),CloseSquare(1:19-1:20),Comma(1:20-1:21),OpenSquare(1:22-1:23),StringStart(1:23-1:24),StringPart(1:24-1:29),StringEnd(1:29-1:30),CloseSquare(1:30-1:31),CloseSquare(1:31-1:32),CloseSquare(1:32-1:33),EndOfFile(1:33-1:33),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.33
	(e-list @1.2-1.4)
	(e-list @1.6-1.15
		(e-list @1.7-1.9)
		(e-list @1.11-1.14
			(e-int @1.12-1.13 (raw "1"))))
	(e-list @1.17-1.32
		(e-list @1.18-1.20)
		(e-list @1.22-1.31
			(e-string @1.23-1.30
				(e-string-part @1.24-1.29 (raw "hello"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-1.33 (elem-var 73) (id 83)
	(elems
		(e-empty_list @1.2-1.4)
		(e-list @1.6-1.15 (elem-var 74)
			(elems
				(e-empty_list @1.7-1.9)
				(e-list @1.11-1.14 (elem-var 75)
					(elems
						(e-int @1.12-1.13 (value "1"))))))
		(e-list @1.17-1.32 (elem-var 78)
			(elems
				(e-empty_list @1.18-1.20)
				(e-list @1.22-1.31 (elem-var 80)
					(elems
						(e-string @1.23-1.30
							(e-literal @1.24-1.29 (string "hello")))))))))
~~~
# TYPES
~~~clojure
(expr (id 83) (type "List(Error)"))
~~~
