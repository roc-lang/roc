# META
~~~ini
description=Heterogeneous list causes type mismatch
type=expr
~~~
# SOURCE
~~~roc
[1, "hello", 3.14]
~~~
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**can_list_heterogeneous.md:1:5:1:12:**
```roc
[1, "hello", 3.14]
```

It is of type:
    _Str_

But you are trying to use it as:
    _Num(*)_

**INCOMPATIBLE LIST ELEMENTS**
This list contains elements with incompatible types:
**can_list_heterogeneous.md:1:1:1:19:**
```roc
[1, "hello", 3.14]
```

The element
    `1`
has the type
    _Num(*)_

However, the element
    `"hello"`
has the incompatible type:
    _Str_

All elements in a list must have compatible types.

**TYPE MISMATCH**
This expression is used in an unexpected way:
**can_list_heterogeneous.md:1:14:1:18:**
```roc
[1, "hello", 3.14]
```

It is of type:
    _Frac(*)_

But you are trying to use it as:
    _StrError_

# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),StringStart(1:5-1:6),StringPart(1:6-1:11),StringEnd(1:11-1:12),Comma(1:12-1:13),Float(1:14-1:18),CloseSquare(1:18-1:19),EndOfFile(1:19-1:19),
~~~
# PARSE
~~~clojure
(e-list @1-1-1-19
	(e-int @1-2-1-3 (raw "1"))
	(e-string @1-5-1-12
		(e-string-part @1-6-1-11 (raw "hello")))
	(e-frac @1-14-1-18 (raw "3.14")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1-1-1-19 (elem-var 72) (id 76)
	(elems
		(e-int @1-2-1-3 (value "1"))
		(e-string @1-5-1-12
			(e-literal @1-6-1-11 (string "hello")))
		(e-dec-small @1-14-1-18 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
~~~
# TYPES
~~~clojure
(expr (id 76) (type "List(Error)"))
~~~
