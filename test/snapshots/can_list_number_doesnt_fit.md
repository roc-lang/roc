# META
~~~ini
description=List with number literal that doesn't fit in inferred type
type=expr
~~~
# SOURCE
~~~roc
[1u8, 2u8, 300]
~~~
# EXPECTED
DEPRECATED NUMBER SUFFIX - can_list_number_doesnt_fit.md:1:2:1:5
DEPRECATED NUMBER SUFFIX - can_list_number_doesnt_fit.md:1:7:1:10
# PROBLEMS
**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**can_list_number_doesnt_fit.md:1:2:1:5:**
```roc
[1u8, 2u8, 300]
```
 ^^^

The `u8` suffix is no longer supported. Use `1.U8` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**can_list_number_doesnt_fit.md:1:7:1:10:**
```roc
[1u8, 2u8, 300]
```
      ^^^

The `u8` suffix is no longer supported. Use `2.U8` instead.

# TOKENS
~~~zig
OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "1u8"))
	(e-int (raw "2u8"))
	(e-int (raw "300")))
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
		(e-num (value "2"))
		(e-num (value "300"))))
~~~
# TYPES
~~~clojure
(expr (type "List(U8)"))
~~~
