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
MISSING METHOD - can_nested_heterogeneous_lists.md:1:3:1:4
MISSING METHOD - can_nested_heterogeneous_lists.md:1:20:1:21
# PROBLEMS
**MISSING METHOD**
This **from_num_literal** method is being called on the type **Str**, which has no method with that name:
**can_nested_heterogeneous_lists.md:1:3:1:4:**
```roc
[[1, "hello"], [2, 3]]
```
  ^


**Hint: **For this to work, the type would need to have a method named **from_num_literal** associated with it in the type's declaration.

**MISSING METHOD**
This **from_num_literal** method is being called on the type **Str**, which has no method with that name:
**can_nested_heterogeneous_lists.md:1:20:1:21:**
```roc
[[1, "hello"], [2, 3]]
```
                   ^


**Hint: **For this to work, the type would need to have a method named **from_num_literal** associated with it in the type's declaration.

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
