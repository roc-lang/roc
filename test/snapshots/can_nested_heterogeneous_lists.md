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
    

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
OpenSquare,OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,CloseSquare,Comma,OpenSquare,Int,Comma,Int,CloseSquare,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-list
		(e-int (raw "1"))
		(e-string
			(e-string-part (raw "hello"))))
	(e-list
		(e-int (raw "2"))
		(e-int (raw "3"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-list
			(elems
				(e-num (value "1"))
				(e-string
					(e-literal (string "hello")))))
		(e-list
			(elems
				(e-num (value "2"))
				(e-num (value "3"))))))
~~~
# TYPES
~~~clojure
(expr (type "List(List(Error))"))
~~~
