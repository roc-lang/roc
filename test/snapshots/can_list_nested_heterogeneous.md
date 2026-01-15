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
TYPE MISMATCH - can_list_nested_heterogeneous.md:1:7:1:8
# PROBLEMS
**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**can_list_nested_heterogeneous.md:1:7:1:8:**
```roc
[[], [1], ["hello"]]
```
      ^

The type was determined to be non-numeric here:
**can_list_nested_heterogeneous.md:1:12:1:19:**
```roc
[[], [1], ["hello"]]
```
           ^^^^^^^

Other code expects this to have the type:

    Str

# TOKENS
~~~zig
OpenSquare,OpenSquare,CloseSquare,Comma,OpenSquare,Int,CloseSquare,Comma,OpenSquare,StringStart,StringPart,StringEnd,CloseSquare,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-list)
	(e-list
		(e-int (raw "1")))
	(e-list
		(e-string
			(e-string-part (raw "hello")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-empty_list)
		(e-list
			(elems
				(e-num (value "1"))))
		(e-list
			(elems
				(e-string
					(e-literal (string "hello")))))))
~~~
# TYPES
~~~clojure
(expr (type "List(List(Str))"))
~~~
