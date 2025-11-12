# META
~~~ini
description=Heterogeneous list causes type mismatch
type=expr
~~~
# SOURCE
~~~roc
[1, "hello", 3.14]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_heterogeneous.md:1:5:1:5
TYPE DOES NOT HAVE METHODS - can_list_heterogeneous.md:1:2:1:3
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The second and third elements in this list have incompatible types:
**can_list_heterogeneous.md:1:5:**
```roc
[1, "hello", 3.14]
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
**can_list_heterogeneous.md:1:2:1:3:**
```roc
[1, "hello", 3.14]
```
 ^

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
	(e-int (raw "1"))
	(e-string
		(e-string-part (raw "hello")))
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
		(e-num (value "1"))
		(e-string
			(e-literal (string "hello")))
		(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
~~~
# TYPES
~~~clojure
(expr (type "List(Error)"))
~~~
