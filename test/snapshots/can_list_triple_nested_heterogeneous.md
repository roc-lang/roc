# META
~~~ini
description=Triply-nested heterogeneous list causes type mismatch
type=expr
~~~
# SOURCE
~~~roc
[[], [[], [1]], [[], ["hello"]]]
~~~
# EXPECTED
TYPE DOES NOT HAVE METHODS - can_list_triple_nested_heterogeneous.md:1:12:1:13
# PROBLEMS
**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_int_digits` on a type that doesn't support methods:
**can_list_triple_nested_heterogeneous.md:1:12:1:13:**
```roc
[[], [[], [1]], [[], ["hello"]]]
```
           ^

This type doesn't support methods:
    _Str_



# TOKENS
~~~zig
OpenSquare,OpenSquare,CloseSquare,Comma,OpenSquare,OpenSquare,CloseSquare,Comma,OpenSquare,Int,CloseSquare,CloseSquare,Comma,OpenSquare,OpenSquare,CloseSquare,Comma,OpenSquare,StringStart,StringPart,StringEnd,CloseSquare,CloseSquare,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-list)
	(e-list
		(e-list)
		(e-list
			(e-int (raw "1"))))
	(e-list
		(e-list)
		(e-list
			(e-string
				(e-string-part (raw "hello"))))))
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
				(e-empty_list)
				(e-list
					(elems
						(e-num (value "1"))))))
		(e-list
			(elems
				(e-empty_list)
				(e-list
					(elems
						(e-string
							(e-literal (string "hello")))))))))
~~~
# TYPES
~~~clojure
(expr (type "List(List(List(Str)))"))
~~~
