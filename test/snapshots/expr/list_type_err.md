# META
~~~ini
description=List with integer literals
type=expr
~~~
# SOURCE
~~~roc
[1, 2, "hello"]
~~~
# EXPECTED
TYPE MISMATCH - list_type_err.md:1:5:1:6
# PROBLEMS
**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**list_type_err.md:1:5:1:6:**
```roc
[1, 2, "hello"]
```
    ^

The type was determined to be non-numeric here:
**list_type_err.md:1:8:1:15:**
```roc
[1, 2, "hello"]
```
       ^^^^^^^

Other code expects this to have the type:

    Str

# TOKENS
~~~zig
OpenSquare,Int,Comma,Int,Comma,StringStart,StringPart,StringEnd,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "1"))
	(e-int (raw "2"))
	(e-string
		(e-string-part (raw "hello"))))
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
		(e-string
			(e-literal (string "hello")))))
~~~
# TYPES
~~~clojure
(expr (type "List(Str)"))
~~~
