# META
~~~ini
description=List with number literal that doesn't fit in inferred type
type=expr
~~~
# SOURCE
~~~roc
[1u8, 2u8, 300]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_number_doesnt_fit.md:1:7:1:7
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The second and third elements in this list have incompatible types:
**can_list_number_doesnt_fit.md:1:7:**
```roc
[1u8, 2u8, 300]
```
      ^^^  ^^^

The second element has this type:
    _U8_

However, the third element has this type:
    _Num(_size)_

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:5),Comma(1:5-1:6),Int(1:7-1:10),Comma(1:10-1:11),Int(1:12-1:15),CloseSquare(1:15-1:16),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.16
	(e-int @1.2-1.5 (raw "1u8"))
	(e-int @1.7-1.10 (raw "2u8"))
	(e-int @1.12-1.15 (raw "300")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-1.16
	(elems
		(e-int @1.2-1.5 (value "1"))
		(e-int @1.7-1.10 (value "2"))
		(e-int @1.12-1.15 (value "300"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.16 (type "List(Error)"))
~~~
