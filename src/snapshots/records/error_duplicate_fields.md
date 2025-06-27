# META
~~~ini
description=Record with duplicate field names (error case)
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
~~~
# PROBLEMS
**DUPLICATE RECORD FIELD**
The record field ``name`` appears more than once in this record.

This field is duplicated here:
**error_duplicate_fields.md:1:27:1:31:**
```roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
```

The field ``name`` was first defined here:
**error_duplicate_fields.md:1:3:1:7:**
```roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
```

Record fields must have unique names. Consider renaming one of these fields or removing the duplicate.

**DUPLICATE RECORD FIELD**
The record field ``age`` appears more than once in this record.

This field is duplicated here:
**error_duplicate_fields.md:1:68:1:71:**
```roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
```

The field ``age`` was first defined here:
**error_duplicate_fields.md:1:18:1:21:**
```roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
```

Record fields must have unique names. Consider renaming one of these fields or removing the duplicate.

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:7),OpColon(1:7-1:8),StringStart(1:9-1:10),StringPart(1:10-1:15),StringEnd(1:15-1:16),Comma(1:16-1:17),LowerIdent(1:18-1:21),OpColon(1:21-1:22),Int(1:23-1:25),Comma(1:25-1:26),LowerIdent(1:27-1:31),OpColon(1:31-1:32),StringStart(1:33-1:34),StringPart(1:34-1:37),StringEnd(1:37-1:38),Comma(1:38-1:39),LowerIdent(1:40-1:45),OpColon(1:45-1:46),StringStart(1:47-1:48),StringPart(1:48-1:65),StringEnd(1:65-1:66),Comma(1:66-1:67),LowerIdent(1:68-1:71),OpColon(1:71-1:72),Int(1:73-1:75),CloseCurly(1:76-1:77),EndOfFile(1:77-1:77),
~~~
# PARSE
~~~clojure
(e-record @1-1-1-77
	(field (field "name") (optional false)
		(e-string @1-9-1-16
			(e-string-part @1-10-1-15 (raw "Alice"))))
	(field (field "age") (optional false)
		(e-int @1-23-1-25 (raw "30")))
	(field (field "name") (optional false)
		(e-string @1-33-1-38
			(e-string-part @1-34-1-37 (raw "Bob"))))
	(field (field "email") (optional false)
		(e-string @1-47-1-66
			(e-string-part @1-48-1-65 (raw "alice@example.com"))))
	(field (field "age") (optional false)
		(e-int @1-73-1-75 (raw "25"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record @1-1-1-77 (ext-var 83) (id 84)
	(fields
		(field (name "name")
			(e-string @1-9-1-16
				(e-literal @1-10-1-15 (string "Alice"))))
		(field (name "age")
			(e-int @1-23-1-25 (value "30")))
		(field (name "email")
			(e-string @1-47-1-66
				(e-literal @1-48-1-65 (string "alice@example.com"))))))
~~~
# TYPES
~~~clojure
(expr (id 84) (type "{ name: Str, age: Num(*), email: Str }"))
~~~