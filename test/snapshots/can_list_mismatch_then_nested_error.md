# META
~~~ini
description=List with type mismatch followed by nested heterogeneous list
type=expr
~~~
# SOURCE
~~~roc
[1, "hello", [3, "world"]]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_mismatch_then_nested_error.md:1:2:1:2
INCOMPATIBLE LIST ELEMENTS - can_list_mismatch_then_nested_error.md:1:15:1:15
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The first two elements in this list have incompatible types:
**can_list_mismatch_then_nested_error.md:1:2:**
```roc
[1, "hello", [3, "world"]]
```
 ^  ^^^^^^^

The first element has this type:
    _Num(_size)_

However, the second element has this type:
    

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

**INCOMPATIBLE LIST ELEMENTS**
The two elements in this list have incompatible types:
**can_list_mismatch_then_nested_error.md:1:15:**
```roc
[1, "hello", [3, "world"]]
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
OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,Comma,OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,CloseSquare,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "1"))
	(e-string
		(e-string-part (raw "hello")))
	(e-list
		(e-int (raw "3"))
		(e-string
			(e-string-part (raw "world")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-num (value "1"))
		(e-string
			(e-literal (string "hello")))
		(e-list
			(elems
				(e-num (value "3"))
				(e-string
					(e-literal (string "world")))))))
~~~
# TYPES
~~~clojure
(expr (type "List(Error)"))
~~~
