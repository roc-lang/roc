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
    _Num(Int(*))_

# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),Int(1:5-1:6),Comma(1:6-1:7),StringStart(1:8-1:9),StringPart(1:9-1:14),StringEnd(1:14-1:15),CloseSquare(1:15-1:16),EndOfFile(1:16-1:16),
~~~
# PARSE
~~~clojure
(list (1:1-1:16)
	(int (1:2-1:3) "1")
	(int (1:5-1:6) "2")
	(string (1:8-1:15) (string_part (1:9-1:14) "hello")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_list (1:1-1:16)
	(elem_var 80)
	(elems
		(e_int (1:2-1:3)
			(int_var 73)
			(precision_var 72)
			(literal "1")
			(value "TODO")
			(bound "u8"))
		(e_int (1:5-1:6)
			(int_var 76)
			(precision_var 75)
			(literal "2")
			(value "TODO")
			(bound "u8"))
		(e_string (1:8-1:15) (e_literal (1:9-1:14) "hello"))))
~~~
# TYPES
~~~clojure
(expr 81 (type "List(Error)"))
~~~