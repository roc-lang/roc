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
TYPE DOES NOT HAVE METHODS - can_list_nested_heterogeneous.md:1:7:1:8
# PROBLEMS
**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_int_digits` on a type that doesn't support methods:
**can_list_nested_heterogeneous.md:1:7:1:8:**
```roc
[[], [1], ["hello"]]
```
      ^

This type doesn't support methods:
    _Str_



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
