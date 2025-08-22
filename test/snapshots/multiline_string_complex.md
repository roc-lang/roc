# META
~~~ini
description=multiline_string_complex
type=file
~~~
# SOURCE
~~~roc
module []

value1 = """This is a "string" with just one line

value2 = 
	"""This is a "string" with just one line

value3 = """This is a string
	"""With multiple lines
	"""${value1}

value4 = 
	"""This is a string
	# A comment in between
	"""With multiple lines
	"""${value2}

value5 = {
	a: """Multiline
	,
	b: (

		"""Multiline
		,
		"""Multiline
		,
	),
	c: [
		"""multiline
		,
	],
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:7),OpAssign(3:8-3:9),MultilineStringStart(3:10-3:13),StringPart(3:13-3:50),
LowerIdent(5:1-5:7),OpAssign(5:8-5:9),
MultilineStringStart(6:2-6:5),StringPart(6:5-6:42),
LowerIdent(8:1-8:7),OpAssign(8:8-8:9),MultilineStringStart(8:10-8:13),StringPart(8:13-8:29),
MultilineStringStart(9:2-9:5),StringPart(9:5-9:24),
MultilineStringStart(10:2-10:5),StringPart(10:5-10:5),OpenStringInterpolation(10:5-10:7),LowerIdent(10:7-10:13),CloseStringInterpolation(10:13-10:14),StringPart(10:14-10:14),
LowerIdent(12:1-12:7),OpAssign(12:8-12:9),
MultilineStringStart(13:2-13:5),StringPart(13:5-13:21),
MultilineStringStart(15:2-15:5),StringPart(15:5-15:24),
MultilineStringStart(16:2-16:5),StringPart(16:5-16:5),OpenStringInterpolation(16:5-16:7),LowerIdent(16:7-16:13),CloseStringInterpolation(16:13-16:14),StringPart(16:14-16:14),
LowerIdent(18:1-18:7),OpAssign(18:8-18:9),OpenCurly(18:10-18:11),
LowerIdent(19:2-19:3),OpColon(19:3-19:4),MultilineStringStart(19:5-19:8),StringPart(19:8-19:17),
Comma(20:2-20:3),
LowerIdent(21:2-21:3),OpColon(21:3-21:4),OpenRound(21:5-21:6),
MultilineStringStart(23:3-23:6),StringPart(23:6-23:15),
Comma(24:3-24:4),
MultilineStringStart(25:3-25:6),StringPart(25:6-25:15),
Comma(26:3-26:4),
CloseRound(27:2-27:3),Comma(27:3-27:4),
LowerIdent(28:2-28:3),OpColon(28:3-28:4),OpenSquare(28:5-28:6),
MultilineStringStart(29:3-29:6),StringPart(29:6-29:15),
Comma(30:3-30:4),
CloseSquare(31:2-31:3),Comma(31:3-31:4),
CloseCurly(32:1-32:2),EndOfFile(32:2-32:2),
~~~
# PARSE
~~~clojure
(file @1.1-32.2
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @3.1-3.50
			(p-ident @3.1-3.7 (raw "value1"))
			(e-multiline-string @3.10-3.50
				(e-string-part @3.13-3.50 (raw "This is a "string" with just one line"))))
		(s-decl @5.1-6.42
			(p-ident @5.1-5.7 (raw "value2"))
			(e-multiline-string @6.2-6.42
				(e-string-part @6.5-6.42 (raw "This is a "string" with just one line"))))
		(s-decl @8.1-10.14
			(p-ident @8.1-8.7 (raw "value3"))
			(e-multiline-string @8.10-10.14
				(e-string-part @8.13-8.29 (raw "This is a string"))
				(e-string-part @9.5-9.24 (raw "With multiple lines"))
				(e-string-part @10.5-10.5 (raw ""))
				(e-ident @10.7-10.13 (raw "value1"))
				(e-string-part @10.14-10.14 (raw ""))))
		(s-decl @12.1-16.14
			(p-ident @12.1-12.7 (raw "value4"))
			(e-multiline-string @13.2-16.14
				(e-string-part @13.5-13.21 (raw "This is a string"))
				(e-string-part @15.5-15.24 (raw "With multiple lines"))
				(e-string-part @16.5-16.5 (raw ""))
				(e-ident @16.7-16.13 (raw "value2"))
				(e-string-part @16.14-16.14 (raw ""))))
		(s-decl @18.1-32.2
			(p-ident @18.1-18.7 (raw "value5"))
			(e-record @18.10-32.2
				(field (field "a")
					(e-multiline-string @19.5-19.17
						(e-string-part @19.8-19.17 (raw "Multiline"))))
				(field (field "b")
					(e-tuple @21.5-27.3
						(e-multiline-string @23.3-23.15
							(e-string-part @23.6-23.15 (raw "Multiline")))
						(e-multiline-string @25.3-25.15
							(e-string-part @25.6-25.15 (raw "Multiline")))))
				(field (field "c")
					(e-list @28.5-31.3
						(e-multiline-string @29.3-29.15
							(e-string-part @29.6-29.15 (raw "multiline")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.7 (ident "value1"))
		(e-string @3.10-3.50
			(e-literal @3.13-3.50 (string "This is a "string" with just one line"))))
	(d-let
		(p-assign @5.1-5.7 (ident "value2"))
		(e-string @6.2-6.42
			(e-literal @6.5-6.42 (string "This is a "string" with just one line"))))
	(d-let
		(p-assign @8.1-8.7 (ident "value3"))
		(e-string @8.10-10.14
			(e-literal @8.13-8.29 (string "This is a string"))
			(e-literal @9.2-9.5 (string "\n"))
			(e-literal @9.5-9.24 (string "With multiple lines"))
			(e-literal @10.2-10.5 (string "\n"))
			(e-lookup-local @10.7-10.13
				(p-assign @3.1-3.7 (ident "value1")))))
	(d-let
		(p-assign @12.1-12.7 (ident "value4"))
		(e-string @13.2-16.14
			(e-literal @13.5-13.21 (string "This is a string"))
			(e-literal @15.2-15.5 (string "\n"))
			(e-literal @15.5-15.24 (string "With multiple lines"))
			(e-literal @16.2-16.5 (string "\n"))
			(e-lookup-local @16.7-16.13
				(p-assign @5.1-5.7 (ident "value2")))))
	(d-let
		(p-assign @18.1-18.7 (ident "value5"))
		(e-record @18.10-32.2
			(fields
				(field (name "a")
					(e-string @19.5-19.17
						(e-literal @19.8-19.17 (string "Multiline"))))
				(field (name "b")
					(e-tuple @21.5-27.3
						(elems
							(e-string @23.3-23.15
								(e-literal @23.6-23.15 (string "Multiline")))
							(e-string @25.3-25.15
								(e-literal @25.6-25.15 (string "Multiline"))))))
				(field (name "c")
					(e-list @28.5-31.3
						(elems
							(e-string @29.3-29.15
								(e-literal @29.6-29.15 (string "multiline"))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.7 (type "Str"))
		(patt @5.1-5.7 (type "Str"))
		(patt @8.1-8.7 (type "Str"))
		(patt @12.1-12.7 (type "Str"))
		(patt @18.1-18.7 (type "{ a: Str, b: (Str, Str), c: List(Str) }")))
	(expressions
		(expr @3.10-3.50 (type "Str"))
		(expr @6.2-6.42 (type "Str"))
		(expr @8.10-10.14 (type "Str"))
		(expr @13.2-16.14 (type "Str"))
		(expr @18.10-32.2 (type "{ a: Str, b: (Str, Str), c: List(Str) }"))))
~~~
