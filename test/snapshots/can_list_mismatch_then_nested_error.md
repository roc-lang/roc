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
TYPE MISMATCH - can_list_mismatch_then_nested_error.md:1:2:1:3
TYPE MISMATCH - can_list_mismatch_then_nested_error.md:1:15:1:16
TYPE MISMATCH - can_list_mismatch_then_nested_error.md:1:14:1:26
# PROBLEMS
**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**can_list_mismatch_then_nested_error.md:1:2:1:3:**
```roc
[1, "hello", [3, "world"]]
```
 ^

The type was determined to be non-numeric here:
**can_list_mismatch_then_nested_error.md:1:5:1:12:**
```roc
[1, "hello", [3, "world"]]
```
    ^^^^^^^

Other code expects this to have the type:

    Str

**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**can_list_mismatch_then_nested_error.md:1:15:1:16:**
```roc
[1, "hello", [3, "world"]]
```
              ^

The type was determined to be non-numeric here:
**can_list_mismatch_then_nested_error.md:1:18:1:25:**
```roc
[1, "hello", [3, "world"]]
```
                 ^^^^^^^

Other code expects this to have the type:

    Str

**TYPE MISMATCH**
The second and third elements in this list have incompatible types:
**can_list_mismatch_then_nested_error.md:1:14:1:26:**
```roc
[1, "hello", [3, "world"]]
```
             ^^^^^^^^^^^^

The second element has this type:

    Str

However, the third element has this type:

    List(Str)

All elements in a list must have compatible types.
__Note:__ You can wrap each element in a tag to make them compatible.
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
