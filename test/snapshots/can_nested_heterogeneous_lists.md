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
TYPE MISMATCH - can_nested_heterogeneous_lists.md:1:3:1:4
TYPE MISMATCH - can_nested_heterogeneous_lists.md:1:20:1:21
# PROBLEMS
**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**can_nested_heterogeneous_lists.md:1:3:1:4:**
```roc
[[1, "hello"], [2, 3]]
```
  ^

The type was determined to be non-numeric here:
**can_nested_heterogeneous_lists.md:1:6:1:13:**
```roc
[[1, "hello"], [2, 3]]
```
     ^^^^^^^

Other code expects this to have the type:

    Str

**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**can_nested_heterogeneous_lists.md:1:20:1:21:**
```roc
[[1, "hello"], [2, 3]]
```
                   ^

The type was determined to be non-numeric here:
**can_nested_heterogeneous_lists.md:1:17:1:18:**
```roc
[[1, "hello"], [2, 3]]
```
                ^

Other code expects this to have the type:

    Str

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
(expr (type "List(List(Str))"))
~~~
