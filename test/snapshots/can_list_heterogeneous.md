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
TYPE MISMATCH - can_list_heterogeneous.md:1:2:1:3
TYPE MISMATCH - can_list_heterogeneous.md:1:14:1:18
# PROBLEMS
**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**can_list_heterogeneous.md:1:2:1:3:**
```roc
[1, "hello", 3.14]
```
 ^

The type was determined to be non-numeric here:
**can_list_heterogeneous.md:1:5:1:12:**
```roc
[1, "hello", 3.14]
```
    ^^^^^^^

Other code expects this to have the type:

    Str

**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**can_list_heterogeneous.md:1:14:1:18:**
```roc
[1, "hello", 3.14]
```
             ^^^^

Other code expects this to have the type:

    Str

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
(expr (type "List(Str)"))
~~~
