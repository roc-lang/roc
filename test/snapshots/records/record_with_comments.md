# META
~~~ini
description=Record creation with comments
type=expr
~~~
# SOURCE
~~~roc
{
	# comment1
	..item,

	# comment2

	person: { name: "Alice", age: 30 }, # comment3
	address: {
		# comment4
		street: "123 Main St",
		# comment5

		city: "Springfield",
		coordinates: { lat: 42.1234, lng: -71.5678 },
	},
	contact: {

		# comment6
		email: "alice@example.com",
		phone: { home: "555-1234", work: "555-5678" }, # comment7
	},
	# comment8
}
~~~
# EXPECTED
UNDEFINED VARIABLE - record_with_comments.md:3:4:3:8
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `item` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_with_comments.md:3:4:3:8:**
```roc
	..item,
```
	  ^^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),
DoubleDot(3:2-3:4),LowerIdent(3:4-3:8),Comma(3:8-3:9),
LowerIdent(7:2-7:8),OpColon(7:8-7:9),OpenCurly(7:10-7:11),LowerIdent(7:12-7:16),OpColon(7:16-7:17),StringStart(7:18-7:19),StringPart(7:19-7:24),StringEnd(7:24-7:25),Comma(7:25-7:26),LowerIdent(7:27-7:30),OpColon(7:30-7:31),Int(7:32-7:34),CloseCurly(7:35-7:36),Comma(7:36-7:37),
LowerIdent(8:2-8:9),OpColon(8:9-8:10),OpenCurly(8:11-8:12),
LowerIdent(10:3-10:9),OpColon(10:9-10:10),StringStart(10:11-10:12),StringPart(10:12-10:23),StringEnd(10:23-10:24),Comma(10:24-10:25),
LowerIdent(13:3-13:7),OpColon(13:7-13:8),StringStart(13:9-13:10),StringPart(13:10-13:21),StringEnd(13:21-13:22),Comma(13:22-13:23),
LowerIdent(14:3-14:14),OpColon(14:14-14:15),OpenCurly(14:16-14:17),LowerIdent(14:18-14:21),OpColon(14:21-14:22),Float(14:23-14:30),Comma(14:30-14:31),LowerIdent(14:32-14:35),OpColon(14:35-14:36),Float(14:37-14:45),CloseCurly(14:46-14:47),Comma(14:47-14:48),
CloseCurly(15:2-15:3),Comma(15:3-15:4),
LowerIdent(16:2-16:9),OpColon(16:9-16:10),OpenCurly(16:11-16:12),
LowerIdent(19:3-19:8),OpColon(19:8-19:9),StringStart(19:10-19:11),StringPart(19:11-19:28),StringEnd(19:28-19:29),Comma(19:29-19:30),
LowerIdent(20:3-20:8),OpColon(20:8-20:9),OpenCurly(20:10-20:11),LowerIdent(20:12-20:16),OpColon(20:16-20:17),StringStart(20:18-20:19),StringPart(20:19-20:27),StringEnd(20:27-20:28),Comma(20:28-20:29),LowerIdent(20:30-20:34),OpColon(20:34-20:35),StringStart(20:36-20:37),StringPart(20:37-20:45),StringEnd(20:45-20:46),CloseCurly(20:47-20:48),Comma(20:48-20:49),
CloseCurly(21:2-21:3),Comma(21:3-21:4),
CloseCurly(23:1-23:2),
EndOfFile(24:1-24:1),
~~~
# PARSE
~~~clojure
(e-record @1.1-23.2
	(ext
		(e-ident @3.4-3.8 (raw "item")))
	(field (field "person")
		(e-record @7.10-7.36
			(field (field "name")
				(e-string @7.18-7.25
					(e-string-part @7.19-7.24 (raw "Alice"))))
			(field (field "age")
				(e-int @7.32-7.34 (raw "30")))))
	(field (field "address")
		(e-record @8.11-15.3
			(field (field "street")
				(e-string @10.11-10.24
					(e-string-part @10.12-10.23 (raw "123 Main St"))))
			(field (field "city")
				(e-string @13.9-13.22
					(e-string-part @13.10-13.21 (raw "Springfield"))))
			(field (field "coordinates")
				(e-record @14.16-14.47
					(field (field "lat")
						(e-frac @14.23-14.30 (raw "42.1234")))
					(field (field "lng")
						(e-frac @14.37-14.45 (raw "-71.5678")))))))
	(field (field "contact")
		(e-record @16.11-21.3
			(field (field "email")
				(e-string @19.10-19.29
					(e-string-part @19.11-19.28 (raw "alice@example.com"))))
			(field (field "phone")
				(e-record @20.10-20.48
					(field (field "home")
						(e-string @20.18-20.28
							(e-string-part @20.19-20.27 (raw "555-1234"))))
					(field (field "work")
						(e-string @20.36-20.46
							(e-string-part @20.37-20.45 (raw "555-5678")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-23.2
	(ext
		(e-runtime-error (tag "ident_not_in_scope")))
	(fields
		(field (name "person")
			(e-record @7.10-7.36
				(fields
					(field (name "name")
						(e-string @7.18-7.25
							(e-literal @7.19-7.24 (string "Alice"))))
					(field (name "age")
						(e-int @7.32-7.34 (value "30"))))))
		(field (name "address")
			(e-record @8.11-15.3
				(fields
					(field (name "street")
						(e-string @10.11-10.24
							(e-literal @10.12-10.23 (string "123 Main St"))))
					(field (name "city")
						(e-string @13.9-13.22
							(e-literal @13.10-13.21 (string "Springfield"))))
					(field (name "coordinates")
						(e-record @14.16-14.47
							(fields
								(field (name "lat")
									(e-frac-dec @14.23-14.30 (value "42.1234")))
								(field (name "lng")
									(e-frac-dec @14.37-14.45 (value "-71.5678")))))))))
		(field (name "contact")
			(e-record @16.11-21.3
				(fields
					(field (name "email")
						(e-string @19.10-19.29
							(e-literal @19.11-19.28 (string "alice@example.com"))))
					(field (name "phone")
						(e-record @20.10-20.48
							(fields
								(field (name "home")
									(e-string @20.18-20.28
										(e-literal @20.19-20.27 (string "555-1234"))))
								(field (name "work")
									(e-string @20.36-20.46
										(e-literal @20.37-20.45 (string "555-5678"))))))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-23.2 (type "{ person: { name: Str, age: Num(_size) }, address: { street: Str, city: Str, coordinates: { lat: Frac(_size2), lng: Frac(_size3) } }, contact: { email: Str, phone: { home: Str, work: Str } } }"))
~~~
