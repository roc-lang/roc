# META
~~~ini
description=Heterogeneous list where first element is concrete
type=expr
~~~
# SOURCE
~~~roc
[42, "world", 3.14]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_first_concrete.md:1:6:1:6
TYPE DOES NOT HAVE METHODS - can_list_first_concrete.md:1:2:1:4
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The second and third elements in this list have incompatible types:
**can_list_first_concrete.md:1:6:**
```roc
[42, "world", 3.14]
```
     ^^^^^^^  ^^^^

The second element has this type:
    _Str_

However, the third element has this type:
    _Num(Frac(_size))_

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_int_digits` on a type that doesn't support methods:
**can_list_first_concrete.md:1:2:1:4:**
```roc
[42, "world", 3.14]
```
 ^^

This type doesn't support methods:
    _Str_



# TOKENS
~~~zig
OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,Comma,Float,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "42"))
	(e-string
		(e-string-part (raw "world")))
	(e-frac (raw "3.14")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-num (value "42"))
		(e-string
			(e-literal (string "world")))
		(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
~~~
# TYPES
~~~clojure
(expr (type "List(Error)"))
~~~
