# META
~~~ini
description=Function with record parameter destructuring and rest pattern, capture whole record using `as`
type=expr
~~~
# SOURCE
~~~roc
|{ name, age, ..a } as person| { greeting: "Hello ${name}", full_record: person, is_adult: age >= 18 }
~~~
# EXPECTED
UNUSED VARIABLE - function_record_parameter_capture.md:1:15:1:20
# PROBLEMS
**UNUSED VARIABLE**
Variable ``a`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_a` to suppress this warning.
The unused variable is declared here:
**function_record_parameter_capture.md:1:15:1:20:**
```roc
|{ name, age, ..a } as person| { greeting: "Hello ${name}", full_record: person, is_adult: age >= 18 }
```
              ^^^^^


# TOKENS
~~~zig
OpBar(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:4-1:8),Comma(1:8-1:9),LowerIdent(1:10-1:13),Comma(1:13-1:14),DoubleDot(1:15-1:17),LowerIdent(1:17-1:18),CloseCurly(1:19-1:20),KwAs(1:21-1:23),LowerIdent(1:24-1:30),OpBar(1:30-1:31),OpenCurly(1:32-1:33),LowerIdent(1:34-1:42),OpColon(1:42-1:43),StringStart(1:44-1:45),StringPart(1:45-1:51),OpenStringInterpolation(1:51-1:53),LowerIdent(1:53-1:57),CloseStringInterpolation(1:57-1:58),StringPart(1:58-1:58),StringEnd(1:58-1:59),Comma(1:59-1:60),LowerIdent(1:61-1:72),OpColon(1:72-1:73),LowerIdent(1:74-1:80),Comma(1:80-1:81),LowerIdent(1:82-1:90),OpColon(1:90-1:91),LowerIdent(1:92-1:95),OpGreaterThanOrEq(1:96-1:98),Int(1:99-1:101),CloseCurly(1:102-1:103),EndOfFile(1:103-1:103),
~~~
# PARSE
~~~clojure
(e-lambda @1.1-1.103
	(args
		(p-as @1.2-1.30 (name "person")
			(p-record @1.2-1.20
				(field @1.4-1.9 (name "name") (rest false))
				(field @1.10-1.14 (name "age") (rest false))
				(field @1.15-1.20 (name "a") (rest true)))))
	(e-record @1.32-1.103
		(field (field "greeting") (optional false)
			(e-string @1.44-1.59
				(e-string-part @1.45-1.51 (raw "Hello "))
				(e-ident @1.53-1.57 (raw "name"))
				(e-string-part @1.58-1.58 (raw ""))))
		(field (field "full_record") (optional false)
			(e-ident @1.74-1.80 (raw "person")))
		(field (field "is_adult") (optional false)
			(e-binop @1.92-1.103 (op ">=")
				(e-ident @1.92-1.95 (raw "age"))
				(e-int @1.99-1.101 (raw "18"))))))
~~~
# FORMATTED
~~~roc
|{ name, age, ..a } as person| {greeting: "Hello ${name}", full_record: person, is_adult: age >= 18}
~~~
# CANONICALIZE
~~~clojure
(e-lambda @1.1-1.103
	(args
		(p-as @1.2-1.30 (as "person")
			(p-record-destructure @1.2-1.20
				(destructs
					(record-destruct @1.4-1.9 (label "name") (ident "name")
						(required))
					(record-destruct @1.10-1.14 (label "age") (ident "age")
						(required))
					(record-destruct @1.15-1.20 (label "a") (ident "a")
						(required))))))
	(e-record @1.32-1.103
		(fields
			(field (name "greeting")
				(e-string @1.44-1.59
					(e-literal @1.45-1.51 (string "Hello "))
					(e-lookup-local @1.53-1.57
						(pattern @1.4-1.9))
					(e-literal @1.58-1.58 (string ""))))
			(field (name "full_record")
				(e-lookup-local @1.74-1.80
					(pattern @1.2-1.30)))
			(field (name "is_adult")
				(e-binop @1.92-1.103 (op "ge")
					(e-lookup-local @1.92-1.95
						(pattern @1.10-1.14))
					(e-int @1.99-1.101 (value "18")))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.103 (type "* -> { greeting: Str, full_record: *, is_adult: * }"))
~~~
