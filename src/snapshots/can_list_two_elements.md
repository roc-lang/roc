# META
~~~ini
description=List with exactly two incompatible elements
type=expr
~~~
# SOURCE
~~~roc
[1, "hello"]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_two_elements.md:1:2:1:2
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The two elements in this list have incompatible types:
**can_list_two_elements.md:1:2:**
```roc
[1, "hello"]
```
 ^  ^^^^^^^

The first element has this type:
    _Num(*)_

However, the second element has this type:
    _Str_

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),StringStart(1:5-1:6),StringPart(1:6-1:11),StringEnd(1:11-1:12),CloseSquare(1:12-1:13),EndOfFile(1:13-1:13),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.13
	(e-int @1.2-1.3 (raw "1"))
	(e-string @1.5-1.12
		(e-string-part @1.6-1.11 (raw "hello"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-1.13
	(elems
		(e-int @1.2-1.3 (value "1"))
		(e-string @1.5-1.12
			(e-literal @1.6-1.11 (string "hello")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.13 (type "List(Error)"))
~~~
