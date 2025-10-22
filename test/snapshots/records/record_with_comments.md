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
OpenCurly,
DoubleDot,LowerIdent,Comma,
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
	(ext
		(e-ident (raw "item")))
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
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record
	(ext
		(e-runtime-error (tag "ident_not_in_scope")))
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
(expr (type "{ address: { city: Error, coordinates: { lat: Num(Frac(_size)), lng: Num(Frac(_size2)) }, street: Error }, contact: { email: Error, phone: { home: Error, work: Error } }, person: { age: Num(_size3), name: Error }, Error }"))
~~~
