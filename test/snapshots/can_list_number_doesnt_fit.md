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
TYPE MISMATCH - can_list_number_doesnt_fit.md:1:12:1:15
TYPE MISMATCH - can_list_number_doesnt_fit.md:1:12:1:15
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**can_list_number_doesnt_fit.md:1:12:1:15:**
```roc
[1u8, 2u8, 300]
```
           ^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**can_list_number_doesnt_fit.md:1:12:1:15:**
```roc
[1u8, 2u8, 300]
```
           ^^^

It has the type:
    _Try(U8, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(U8, [InvalidNumeral(Str)])_

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
(expr (type "List(Error)"))
~~~
