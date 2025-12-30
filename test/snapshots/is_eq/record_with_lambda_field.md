# META
~~~ini
description=Record with lambda field doesn't support equality - shows which field is ineligible
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: "30", process: |x| x } == { name: "Bob", age: "25", process: |y| y }
~~~
# EXPECTED
TYPE DOES NOT SUPPORT EQUALITY - record_with_lambda_field.md:1:1:1:91
: - :0:0:0:0
# PROBLEMS
**TYPE DOES NOT SUPPORT EQUALITY**
This expression uses **==** or **!=** on a type that doesn't support equality:
**record_with_lambda_field.md:1:1:1:91:**
```roc
{ name: "Alice", age: "30", process: |x| x } == { name: "Bob", age: "25", process: |y| y }
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The type is:

    { age: Str, name: Str, process: a -> a }

This record does not support equality because these fields have types that don't support **is_eq**:

    **process**: _a -> a_
        Function equality is not supported.
**Hint:** Anonymous records only have an **is_eq** method if all of their fields have **is_eq** methods.


# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,OpBar,LowerIdent,OpBar,LowerIdent,CloseCurly,OpEquals,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,OpBar,LowerIdent,OpBar,LowerIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "==")
	(e-record
		(field (field "name")
			(e-string
				(e-string-part (raw "Alice"))))
		(field (field "age")
			(e-string
				(e-string-part (raw "30"))))
		(field (field "process")
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x")))))
	(e-record
		(field (field "name")
			(e-string
				(e-string-part (raw "Bob"))))
		(field (field "age")
			(e-string
				(e-string-part (raw "25"))))
		(field (field "process")
			(e-lambda
				(args
					(p-ident (raw "y")))
				(e-ident (raw "y"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-binop (op "eq")
	(e-record
		(fields
			(field (name "name")
				(e-string
					(e-literal (string "Alice"))))
			(field (name "age")
				(e-string
					(e-literal (string "30"))))
			(field (name "process")
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-lookup-local
						(p-assign (ident "x")))))))
	(e-record
		(fields
			(field (name "name")
				(e-string
					(e-literal (string "Bob"))))
			(field (name "age")
				(e-string
					(e-literal (string "25"))))
			(field (name "process")
				(e-lambda
					(args
						(p-assign (ident "y")))
					(e-lookup-local
						(p-assign (ident "y"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
