# META
~~~ini
description=Let-polymorphism error case - incompatible list elements
type=expr
~~~
# SOURCE
~~~roc
[42, 4.2, "hello"]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - let_polymorphism_error.md:1:6:1:6
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The second and third elements in this list have incompatible types:
**let_polymorphism_error.md:1:6:**
```roc
[42, 4.2, "hello"]
```
     ^^^  ^^^^^^^

The second element has this type:
    _Frac(a)_

However, the third element has this type:
    _Str_

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:4),Comma(1:4-1:5),Float(1:6-1:9),Comma(1:9-1:10),StringStart(1:11-1:12),StringPart(1:12-1:17),StringEnd(1:17-1:18),CloseSquare(1:18-1:19),EndOfFile(1:19-1:19),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.19
	(e-int @1.2-1.4 (raw "42"))
	(e-frac @1.6-1.9 (raw "4.2"))
	(e-string @1.11-1.18
		(e-string-part @1.12-1.17 (raw "hello"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-1.19
	(elems
		(e-int @1.2-1.4 (value "42"))
		(e-dec-small @1.6-1.9 (numerator "42") (denominator-power-of-ten "1") (value "4.2"))
		(e-string @1.11-1.18
			(e-literal @1.12-1.17 (string "hello")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.19 (type "List(Error)"))
~~~
