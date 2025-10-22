# META
~~~ini
description=Record with duplicate field names (error case)
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
~~~
# EXPECTED
DUPLICATE RECORD FIELD - error_duplicate_fields.md:1:27:1:31
DUPLICATE RECORD FIELD - error_duplicate_fields.md:1:68:1:71
# PROBLEMS
**DUPLICATE RECORD FIELD**
The record field `name` appears more than once in this record.

This field is duplicated here:
**error_duplicate_fields.md:1:27:1:31:**
```roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
```
                          ^^^^

The field `name` was first defined here:
**error_duplicate_fields.md:1:3:1:7:**
```roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
```
  ^^^^

Record fields must have unique names. Consider renaming one of these fields or removing the duplicate.

**DUPLICATE RECORD FIELD**
The record field `age` appears more than once in this record.

This field is duplicated here:
**error_duplicate_fields.md:1:68:1:71:**
```roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
```
                                                                   ^^^

The field `age` was first defined here:
**error_duplicate_fields.md:1:18:1:21:**
```roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
```
                 ^^^

Record fields must have unique names. Consider renaming one of these fields or removing the duplicate.

# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "name")
		(e-string
			(e-string-part (raw "Alice"))))
	(field (field "age")
		(e-int (raw "30")))
	(field (field "name")
		(e-string
			(e-string-part (raw "Bob"))))
	(field (field "email")
		(e-string
			(e-string-part (raw "alice@example.com"))))
	(field (field "age")
		(e-int (raw "25"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "name")
			(e-string
				(e-literal (string "Alice"))))
		(field (name "age")
			(e-num (value "30")))
		(field (name "email")
			(e-string
				(e-literal (string "alice@example.com"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ age: Num(_size), email: Error, name: Error }"))
~~~
