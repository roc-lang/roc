# META
~~~ini
description=List with integer literals
type=expr
~~~
# SOURCE
~~~roc
[1, 2, "hello"]
~~~
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**list_type_err.md:1:8:1:15:**
```roc
[1, 2, "hello"]
```

It is of type:
    _Str_

But you are trying to use it as:
    _Num(*)_

# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),Int(1:5-1:6),Comma(1:6-1:7),StringStart(1:8-1:9),StringPart(1:9-1:14),StringEnd(1:14-1:15),CloseSquare(1:15-1:16),EndOfFile(1:16-1:16),
~~~
# PARSE
~~~clojure
(e-list @1-1-1-16
	(e-int @1-2-1-3 (raw "1"))
	(e-int @1-5-1-6 (raw "2"))
	(e-string @1-8-1-15
		(e-string-part @1-9-1-14 (raw "hello"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1-1-1-16 (elem-var 76) (id 77)
	(elems
		(e-int @1-2-1-3 (value "1"))
		(e-int @1-5-1-6 (value "2"))
		(e-string @1-8-1-15
			(e-literal @1-9-1-14 (string "hello")))))
~~~
# TYPES
~~~clojure
(expr (id 77) (type "List(Error)"))
~~~