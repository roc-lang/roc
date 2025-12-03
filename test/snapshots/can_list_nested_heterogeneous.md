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
MISSING METHOD - can_list_nested_heterogeneous.md:1:7:1:8
# PROBLEMS
**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**can_list_nested_heterogeneous.md:1:7:1:8:**
```roc
[[], [1], ["hello"]]
```
      ^

The value's type, which does not have a method named **from_numeral**, is:

    _Str_

**Hint:** For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

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
