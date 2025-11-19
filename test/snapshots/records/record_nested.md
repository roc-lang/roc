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
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,Comma,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,Float,Comma,LowerIdent,OpColon,Float,CloseCurly,Comma,
CloseCurly,Comma,
LowerIdent,OpColon,OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,Comma,
CloseCurly,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "person")
		(e-record
			(field (field "name")
				(e-string
					(e-string-part (raw "Alice"))))
			(field (field "age")
				(e-int (raw "30")))))
	(field (field "address")
		(e-record
			(field (field "street")
				(e-string
					(e-string-part (raw "123 Main St"))))
			(field (field "city")
				(e-string
					(e-string-part (raw "Springfield"))))
			(field (field "coordinates")
				(e-record
					(field (field "lat")
						(e-frac (raw "42.1234")))
					(field (field "lng")
						(e-frac (raw "-71.5678")))))))
	(field (field "contact")
		(e-record
			(field (field "email")
				(e-string
					(e-string-part (raw "alice@example.com"))))
			(field (field "phone")
				(e-record
					(field (field "home")
						(e-string
							(e-string-part (raw "555-1234"))))
					(field (field "work")
						(e-string
							(e-string-part (raw "555-5678")))))))))
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
(e-record
	(fields
		(field (name "person")
			(e-record
				(fields
					(field (name "name")
						(e-string
							(e-literal (string "Alice"))))
					(field (name "age")
						(e-num (value "30"))))))
		(field (name "address")
			(e-record
				(fields
					(field (name "street")
						(e-string
							(e-literal (string "123 Main St"))))
					(field (name "city")
						(e-string
							(e-literal (string "Springfield"))))
					(field (name "coordinates")
						(e-record
							(fields
								(field (name "lat")
									(e-frac-dec (value "42.1234")))
								(field (name "lng")
									(e-frac-dec (value "-71.5678")))))))))
		(field (name "contact")
			(e-record
				(fields
					(field (name "email")
						(e-string
							(e-literal (string "alice@example.com"))))
					(field (name "phone")
						(e-record
							(fields
								(field (name "home")
									(e-string
										(e-literal (string "555-1234"))))
								(field (name "work")
									(e-string
										(e-literal (string "555-5678"))))))))))))
~~~
# TYPES
~~~clojure
(expr (type "{ address: { city: Str, coordinates: { lat: Num(Frac(_size)), lng: Num(Frac(_size2)) }, street: Str }, contact: { email: Str, phone: { home: Str, work: Str } }, person: { age: Num(_size3), name: Str } }"))
~~~
