# META
~~~ini
description=Nested record creation
type=expr
~~~
# SOURCE
~~~roc
{
    person: { name: "Alice", age: 30 },
    address: {
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
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:11),OpColon(2:11-2:12),OpenCurly(2:13-2:14),LowerIdent(2:15-2:19),OpColon(2:19-2:20),StringStart(2:21-2:22),StringPart(2:22-2:27),StringEnd(2:27-2:28),Comma(2:28-2:29),LowerIdent(2:30-2:33),OpColon(2:33-2:34),Int(2:35-2:37),CloseCurly(2:38-2:39),Comma(2:39-2:40),Newline(1:1-1:1),
LowerIdent(3:5-3:12),OpColon(3:12-3:13),OpenCurly(3:14-3:15),Newline(1:1-1:1),
LowerIdent(4:9-4:15),OpColon(4:15-4:16),StringStart(4:17-4:18),StringPart(4:18-4:29),StringEnd(4:29-4:30),Comma(4:30-4:31),Newline(1:1-1:1),
LowerIdent(5:9-5:13),OpColon(5:13-5:14),StringStart(5:15-5:16),StringPart(5:16-5:27),StringEnd(5:27-5:28),Comma(5:28-5:29),Newline(1:1-1:1),
LowerIdent(6:9-6:20),OpColon(6:20-6:21),OpenCurly(6:22-6:23),LowerIdent(6:24-6:27),OpColon(6:27-6:28),Float(6:29-6:36),Comma(6:36-6:37),LowerIdent(6:38-6:41),OpColon(6:41-6:42),Float(6:43-6:51),CloseCurly(6:52-6:53),Comma(6:53-6:54),Newline(1:1-1:1),
CloseCurly(7:5-7:6),Comma(7:6-7:7),Newline(1:1-1:1),
LowerIdent(8:5-8:12),OpColon(8:12-8:13),OpenCurly(8:14-8:15),Newline(1:1-1:1),
LowerIdent(9:9-9:14),OpColon(9:14-9:15),StringStart(9:16-9:17),StringPart(9:17-9:34),StringEnd(9:34-9:35),Comma(9:35-9:36),Newline(1:1-1:1),
LowerIdent(10:9-10:14),OpColon(10:14-10:15),OpenCurly(10:16-10:17),LowerIdent(10:18-10:22),OpColon(10:22-10:23),StringStart(10:24-10:25),StringPart(10:25-10:33),StringEnd(10:33-10:34),Comma(10:34-10:35),LowerIdent(10:36-10:40),OpColon(10:40-10:41),StringStart(10:42-10:43),StringPart(10:43-10:51),StringEnd(10:51-10:52),CloseCurly(10:53-10:54),Comma(10:54-10:55),Newline(1:1-1:1),
CloseCurly(11:5-11:6),Comma(11:6-11:7),Newline(1:1-1:1),
CloseCurly(12:1-12:2),EndOfFile(12:2-12:2),
~~~
# PARSE
~~~clojure
(e-record @1.1-12.2
	(field (field "person") (optional false)
		(e-record @2.13-2.39
			(field (field "name") (optional false)
				(e-string @2.21-2.28
					(e-string-part @2.22-2.27 (raw "Alice"))))
			(field (field "age") (optional false)
				(e-int @2.35-2.37 (raw "30")))))
	(field (field "address") (optional false)
		(e-record @3.14-7.6
			(field (field "street") (optional false)
				(e-string @4.17-4.30
					(e-string-part @4.18-4.29 (raw "123 Main St"))))
			(field (field "city") (optional false)
				(e-string @5.15-5.28
					(e-string-part @5.16-5.27 (raw "Springfield"))))
			(field (field "coordinates") (optional false)
				(e-record @6.22-6.53
					(field (field "lat") (optional false)
						(e-frac @6.29-6.36 (raw "42.1234")))
					(field (field "lng") (optional false)
						(e-frac @6.43-6.51 (raw "-71.5678")))))))
	(field (field "contact") (optional false)
		(e-record @8.14-11.6
			(field (field "email") (optional false)
				(e-string @9.16-9.35
					(e-string-part @9.17-9.34 (raw "alice@example.com"))))
			(field (field "phone") (optional false)
				(e-record @10.16-10.54
					(field (field "home") (optional false)
						(e-string @10.24-10.34
							(e-string-part @10.25-10.33 (raw "555-1234"))))
					(field (field "work") (optional false)
						(e-string @10.42-10.52
							(e-string-part @10.43-10.51 (raw "555-5678")))))))))
~~~
# FORMATTED
~~~roc
{
	person: { name: "Alice", age: 30 },
	address: {
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
# CANONICALIZE
~~~clojure
(e-record @1.1-12.2
	(fields
		(field (name "person")
			(e-record @2.13-2.39
				(fields
					(field (name "name")
						(e-string @2.21-2.28
							(e-literal @2.22-2.27 (string "Alice"))))
					(field (name "age")
						(e-int @2.35-2.37 (value "30"))))))
		(field (name "address")
			(e-record @3.14-7.6
				(fields
					(field (name "street")
						(e-string @4.17-4.30
							(e-literal @4.18-4.29 (string "123 Main St"))))
					(field (name "city")
						(e-string @5.15-5.28
							(e-literal @5.16-5.27 (string "Springfield"))))
					(field (name "coordinates")
						(e-record @6.22-6.53
							(fields
								(field (name "lat")
									(e-frac-dec @6.29-6.36 (value "42.1234")))
								(field (name "lng")
									(e-frac-dec @6.43-6.51 (value "-71.5678")))))))))
		(field (name "contact")
			(e-record @8.14-11.6
				(fields
					(field (name "email")
						(e-string @9.16-9.35
							(e-literal @9.17-9.34 (string "alice@example.com"))))
					(field (name "phone")
						(e-record @10.16-10.54
							(fields
								(field (name "home")
									(e-string @10.24-10.34
										(e-literal @10.25-10.33 (string "555-1234"))))
								(field (name "work")
									(e-string @10.42-10.52
										(e-literal @10.43-10.51 (string "555-5678"))))))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-12.2 (type "{ person: { name: Str, age: Num(a) }, address: { street: Str, city: Str, coordinates: { lat: Frac(b), lng: Frac(c) } }, contact: { email: Str, phone: { home: Str, work: Str } } }"))
~~~
