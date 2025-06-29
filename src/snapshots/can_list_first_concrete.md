# META
~~~ini
description=Heterogeneous list where first element is concrete
type=expr
~~~
# SOURCE
~~~roc
[42, "world", 3.14]
~~~
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The first two elements in this list have incompatible types:
**can_list_first_concrete.md:1:2:**
```roc
[42, "world", 3.14]
```
 ^^
     ^^^^^^^

The first element has this type:
    _Num(*)_

However, the second element has this type:
    _Str_

All elements in a list must have compatible types.

# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:4),Comma(1:4-1:5),StringStart(1:6-1:7),StringPart(1:7-1:12),StringEnd(1:12-1:13),Comma(1:13-1:14),Float(1:15-1:19),CloseSquare(1:19-1:20),EndOfFile(1:20-1:20),
~~~
# PARSE
~~~clojure
(e-list @1-1-1-20
	(e-int @1-2-1-4 (raw "42"))
	(e-string @1-6-1-13
		(e-string-part @1-7-1-12 (raw "world")))
	(e-frac @1-15-1-19 (raw "3.14")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1-1-1-20 (elem-var 72) (id 76)
	(elems
		(e-int @1-2-1-4 (value "42"))
		(e-string @1-6-1-13
			(e-literal @1-7-1-12 (string "world")))
		(e-dec-small @1-15-1-19 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
~~~
# TYPES
~~~clojure
(expr (id 76) (type "List(Error)"))
~~~
