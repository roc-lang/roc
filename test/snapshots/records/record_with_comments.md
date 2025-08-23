# META
~~~ini
description=Record creation with comments
type=expr
~~~
# SOURCE
~~~roc
{
	# comment
	..item,
	person: { name: "Alice", age: 30 },
	address: {
		# comment
		street: "123 Main St",
		city: "Springfield",
		coordinates: { lat: 42.1234, lng: -71.5678 },
	},
	contact: {
		email: "alice@example.com",
		phone: { home: "555-1234", work: "555-5678" },
	},
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
LowerIdent(4:2-4:8),OpColon(4:8-4:9),OpenCurly(4:10-4:11),LowerIdent(4:12-4:16),OpColon(4:16-4:17),StringStart(4:18-4:19),StringPart(4:19-4:24),StringEnd(4:24-4:25),Comma(4:25-4:26),LowerIdent(4:27-4:30),OpColon(4:30-4:31),Int(4:32-4:34),CloseCurly(4:35-4:36),Comma(4:36-4:37),
LowerIdent(5:2-5:9),OpColon(5:9-5:10),OpenCurly(5:11-5:12),
LowerIdent(7:3-7:9),OpColon(7:9-7:10),StringStart(7:11-7:12),StringPart(7:12-7:23),StringEnd(7:23-7:24),Comma(7:24-7:25),
LowerIdent(8:3-8:7),OpColon(8:7-8:8),StringStart(8:9-8:10),StringPart(8:10-8:21),StringEnd(8:21-8:22),Comma(8:22-8:23),
LowerIdent(9:3-9:14),OpColon(9:14-9:15),OpenCurly(9:16-9:17),LowerIdent(9:18-9:21),OpColon(9:21-9:22),Float(9:23-9:30),Comma(9:30-9:31),LowerIdent(9:32-9:35),OpColon(9:35-9:36),Float(9:37-9:45),CloseCurly(9:46-9:47),Comma(9:47-9:48),
CloseCurly(10:2-10:3),Comma(10:3-10:4),
LowerIdent(11:2-11:9),OpColon(11:9-11:10),OpenCurly(11:11-11:12),
LowerIdent(12:3-12:8),OpColon(12:8-12:9),StringStart(12:10-12:11),StringPart(12:11-12:28),StringEnd(12:28-12:29),Comma(12:29-12:30),
LowerIdent(13:3-13:8),OpColon(13:8-13:9),OpenCurly(13:10-13:11),LowerIdent(13:12-13:16),OpColon(13:16-13:17),StringStart(13:18-13:19),StringPart(13:19-13:27),StringEnd(13:27-13:28),Comma(13:28-13:29),LowerIdent(13:30-13:34),OpColon(13:34-13:35),StringStart(13:36-13:37),StringPart(13:37-13:45),StringEnd(13:45-13:46),CloseCurly(13:47-13:48),Comma(13:48-13:49),
CloseCurly(14:2-14:3),Comma(14:3-14:4),
CloseCurly(15:1-15:2),EndOfFile(15:2-15:2),
~~~
# PARSE
~~~clojure
(e-record @1.1-15.2
	(ext
		(e-ident @3.4-3.8 (raw "item")))
	(field (field "person")
		(e-record @4.10-4.36
			(field (field "name")
				(e-string @4.18-4.25
					(e-string-part @4.19-4.24 (raw "Alice"))))
			(field (field "age")
				(e-int @4.32-4.34 (raw "30")))))
	(field (field "address")
		(e-record @5.11-10.3
			(field (field "street")
				(e-string @7.11-7.24
					(e-string-part @7.12-7.23 (raw "123 Main St"))))
			(field (field "city")
				(e-string @8.9-8.22
					(e-string-part @8.10-8.21 (raw "Springfield"))))
			(field (field "coordinates")
				(e-record @9.16-9.47
					(field (field "lat")
						(e-frac @9.23-9.30 (raw "42.1234")))
					(field (field "lng")
						(e-frac @9.37-9.45 (raw "-71.5678")))))))
	(field (field "contact")
		(e-record @11.11-14.3
			(field (field "email")
				(e-string @12.10-12.29
					(e-string-part @12.11-12.28 (raw "alice@example.com"))))
			(field (field "phone")
				(e-record @13.10-13.48
					(field (field "home")
						(e-string @13.18-13.28
							(e-string-part @13.19-13.27 (raw "555-1234"))))
					(field (field "work")
						(e-string @13.36-13.46
							(e-string-part @13.37-13.45 (raw "555-5678")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-15.2
	(ext
		(e-runtime-error (tag "ident_not_in_scope")))
	(fields
		(field (name "person")
			(e-record @4.10-4.36
				(fields
					(field (name "name")
						(e-string @4.18-4.25
							(e-literal @4.19-4.24 (string "Alice"))))
					(field (name "age")
						(e-int @4.32-4.34 (value "30"))))))
		(field (name "address")
			(e-record @5.11-10.3
				(fields
					(field (name "street")
						(e-string @7.11-7.24
							(e-literal @7.12-7.23 (string "123 Main St"))))
					(field (name "city")
						(e-string @8.9-8.22
							(e-literal @8.10-8.21 (string "Springfield"))))
					(field (name "coordinates")
						(e-record @9.16-9.47
							(fields
								(field (name "lat")
									(e-frac-dec @9.23-9.30 (value "42.1234")))
								(field (name "lng")
									(e-frac-dec @9.37-9.45 (value "-71.5678")))))))))
		(field (name "contact")
			(e-record @11.11-14.3
				(fields
					(field (name "email")
						(e-string @12.10-12.29
							(e-literal @12.11-12.28 (string "alice@example.com"))))
					(field (name "phone")
						(e-record @13.10-13.48
							(fields
								(field (name "home")
									(e-string @13.18-13.28
										(e-literal @13.19-13.27 (string "555-1234"))))
								(field (name "work")
									(e-string @13.36-13.46
										(e-literal @13.37-13.45 (string "555-5678"))))))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-15.2 (type "{ person: { name: Str, age: Num(_size) }, address: { street: Str, city: Str, coordinates: { lat: Frac(_size2), lng: Frac(_size3) } }, contact: { email: Str, phone: { home: Str, work: Str } } }"))
~~~
