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
INCOMPATIBLE LIST ELEMENTS - can_list_mismatch_then_nested_error.md:1:5:1:5
TYPE DOES NOT HAVE METHODS - can_list_mismatch_then_nested_error.md:1:2:1:3
TYPE DOES NOT HAVE METHODS - can_list_mismatch_then_nested_error.md:1:15:1:16
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The second and third elements in this list have incompatible types:
**can_list_mismatch_then_nested_error.md:1:5:**
```roc
[1, "hello", [3, "world"]]
```
    ^^^^^^^  ^^^^^^^^^^^^

The second element has this type:
    _Str_

However, the third element has this type:
    _List(Str)_

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_int_digits` on a type that doesn't support methods:
**can_list_mismatch_then_nested_error.md:1:2:1:3:**
```roc
[1, "hello", [3, "world"]]
```
 ^

This type doesn't support methods:
    _Str_



**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_int_digits` on a type that doesn't support methods:
**can_list_mismatch_then_nested_error.md:1:15:1:16:**
```roc
[1, "hello", [3, "world"]]
```
              ^

This type doesn't support methods:
    _Str_



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
