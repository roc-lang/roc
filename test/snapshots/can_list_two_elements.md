# META
~~~ini
description=List with exactly two incompatible elements
type=expr
~~~
# SOURCE
~~~roc
[1, "hello"]
~~~
# EXPECTED
MISSING METHOD - can_list_two_elements.md:1:2:1:3
# PROBLEMS
**MISSING METHOD**
This **from_num_literal** method is being called on the type **Str**, which has no method with that name:
**can_list_two_elements.md:1:2:1:3:**
```roc
[1, "hello"]
```
 ^


**Hint: **For this to work, the type would need to have a method named **from_num_literal** associated with it in the type's declaration.

# TOKENS
~~~zig
OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "1"))
	(e-string
		(e-string-part (raw "hello"))))
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
			(e-literal (string "hello")))))
~~~
# TYPES
~~~clojure
(expr (type "List(Str)"))
~~~
