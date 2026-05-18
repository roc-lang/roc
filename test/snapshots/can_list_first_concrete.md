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
TYPE MISMATCH - can_list_first_concrete.md:1:2:1:4
TYPE MISMATCH - can_list_first_concrete.md:1:15:1:19
# PROBLEMS
**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**can_list_first_concrete.md:1:2:1:4:**
```roc
[42, "world", 3.14]
```
 ^^

The type was determined to be non-numeric here:
**can_list_first_concrete.md:1:6:1:13:**
```roc
[42, "world", 3.14]
```
     ^^^^^^^

Other code expects this to have the type:

    Str

**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**can_list_first_concrete.md:1:15:1:19:**
```roc
[42, "world", 3.14]
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
(expr (type "List(Str)"))
~~~
