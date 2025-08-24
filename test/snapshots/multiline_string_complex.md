# META
~~~ini
description=multiline_string_complex
type=file
~~~
# SOURCE
~~~roc
package
	[]
	{
		x: """Multiline
		,
	}

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

x = {
	"""
	"""
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwPackage(1:1-1:8),
OpenSquare(2:2-2:3),CloseSquare(2:3-2:4),
OpenCurly(3:2-3:3),
LowerIdent(4:3-4:4),OpColon(4:4-4:5),MultilineStringStart(4:6-4:9),StringPart(4:9-4:18),
Comma(5:3-5:4),
CloseCurly(6:2-6:3),
LowerIdent(8:1-8:7),OpAssign(8:8-8:9),MultilineStringStart(8:10-8:13),StringPart(8:13-8:50),
LowerIdent(10:1-10:7),OpAssign(10:8-10:9),
MultilineStringStart(11:2-11:5),StringPart(11:5-11:42),
LowerIdent(13:1-13:7),OpAssign(13:8-13:9),MultilineStringStart(13:10-13:13),StringPart(13:13-13:29),
MultilineStringStart(14:2-14:5),StringPart(14:5-14:24),
MultilineStringStart(15:2-15:5),StringPart(15:5-15:5),OpenStringInterpolation(15:5-15:7),LowerIdent(15:7-15:13),CloseStringInterpolation(15:13-15:14),StringPart(15:14-15:14),
LowerIdent(17:1-17:7),OpAssign(17:8-17:9),
MultilineStringStart(18:2-18:5),StringPart(18:5-18:21),
MultilineStringStart(20:2-20:5),StringPart(20:5-20:24),
MultilineStringStart(21:2-21:5),StringPart(21:5-21:5),OpenStringInterpolation(21:5-21:7),LowerIdent(21:7-21:13),CloseStringInterpolation(21:13-21:14),StringPart(21:14-21:14),
LowerIdent(23:1-23:7),OpAssign(23:8-23:9),OpenCurly(23:10-23:11),
LowerIdent(24:2-24:3),OpColon(24:3-24:4),MultilineStringStart(24:5-24:8),StringPart(24:8-24:17),
Comma(25:2-25:3),
LowerIdent(26:2-26:3),OpColon(26:3-26:4),OpenRound(26:5-26:6),
MultilineStringStart(27:3-27:6),StringPart(27:6-27:15),
Comma(28:3-28:4),
MultilineStringStart(29:3-29:6),StringPart(29:6-29:15),
Comma(30:3-30:4),
CloseRound(31:2-31:3),Comma(31:3-31:4),
LowerIdent(32:2-32:3),OpColon(32:3-32:4),OpenSquare(32:5-32:6),
MultilineStringStart(33:3-33:6),StringPart(33:6-33:15),
Comma(34:3-34:4),
CloseSquare(35:2-35:3),Comma(35:3-35:4),
CloseCurly(36:1-36:2),
LowerIdent(38:1-38:2),OpAssign(38:3-38:4),OpenCurly(38:5-38:6),
MultilineStringStart(39:2-39:5),StringPart(39:5-39:5),
MultilineStringStart(40:2-40:5),StringPart(40:5-40:5),
CloseCurly(41:1-41:2),EndOfFile(41:2-41:2),
~~~
# PARSE
~~~clojure
(file @1.1-41.2
	(package @1.1-6.3
		(exposes @2.2-2.4)
		(packages @3.2-6.3
			(record-field @4.3-4.18 (name "x")
				(e-multiline-string @4.6-4.18
					(e-string-part @4.9-4.18 (raw "Multiline"))))))
	(statements
		(s-decl @8.1-8.50
			(p-ident @8.1-8.7 (raw "value1"))
			(e-multiline-string @8.10-8.50
				(e-string-part @8.13-8.50 (raw "This is a "string" with just one line"))))
		(s-decl @10.1-11.42
			(p-ident @10.1-10.7 (raw "value2"))
			(e-multiline-string @11.2-11.42
				(e-string-part @11.5-11.42 (raw "This is a "string" with just one line"))))
		(s-decl @13.1-15.14
			(p-ident @13.1-13.7 (raw "value3"))
			(e-multiline-string @13.10-15.14
				(e-string-part @13.13-13.29 (raw "This is a string"))
				(e-string-part @14.5-14.24 (raw "With multiple lines"))
				(e-string-part @15.5-15.5 (raw ""))
				(e-ident @15.7-15.13 (raw "value1"))
				(e-string-part @15.14-15.14 (raw ""))))
		(s-decl @17.1-21.14
			(p-ident @17.1-17.7 (raw "value4"))
			(e-multiline-string @18.2-21.14
				(e-string-part @18.5-18.21 (raw "This is a string"))
				(e-string-part @20.5-20.24 (raw "With multiple lines"))
				(e-string-part @21.5-21.5 (raw ""))
				(e-ident @21.7-21.13 (raw "value2"))
				(e-string-part @21.14-21.14 (raw ""))))
		(s-decl @23.1-36.2
			(p-ident @23.1-23.7 (raw "value5"))
			(e-record @23.10-36.2
				(field (field "a")
					(e-multiline-string @24.5-24.17
						(e-string-part @24.8-24.17 (raw "Multiline"))))
				(field (field "b")
					(e-tuple @26.5-31.3
						(e-multiline-string @27.3-27.15
							(e-string-part @27.6-27.15 (raw "Multiline")))
						(e-multiline-string @29.3-29.15
							(e-string-part @29.6-29.15 (raw "Multiline")))))
				(field (field "c")
					(e-list @32.5-35.3
						(e-multiline-string @33.3-33.15
							(e-string-part @33.6-33.15 (raw "multiline")))))))
		(s-decl @38.1-41.2
			(p-ident @38.1-38.2 (raw "x"))
			(e-block @38.5-41.2
				(statements
					(e-multiline-string @39.2-40.5
						(e-string-part @39.5-39.5 (raw ""))
						(e-string-part @40.5-40.5 (raw ""))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @8.1-8.7 (ident "value1"))
		(e-string @8.10-8.50
			(e-literal @8.13-8.50 (string "This is a "string" with just one line"))))
	(d-let
		(p-assign @10.1-10.7 (ident "value2"))
		(e-string @11.2-11.42
			(e-literal @11.5-11.42 (string "This is a "string" with just one line"))))
	(d-let
		(p-assign @13.1-13.7 (ident "value3"))
		(e-string @13.10-15.14
			(e-literal @13.13-13.29 (string "This is a string"))
			(e-literal @14.2-14.5 (string "\n"))
			(e-literal @14.5-14.24 (string "With multiple lines"))
			(e-literal @15.2-15.5 (string "\n"))
			(e-lookup-local @15.7-15.13
				(p-assign @8.1-8.7 (ident "value1")))))
	(d-let
		(p-assign @17.1-17.7 (ident "value4"))
		(e-string @18.2-21.14
			(e-literal @18.5-18.21 (string "This is a string"))
			(e-literal @20.2-20.5 (string "\n"))
			(e-literal @20.5-20.24 (string "With multiple lines"))
			(e-literal @21.2-21.5 (string "\n"))
			(e-lookup-local @21.7-21.13
				(p-assign @10.1-10.7 (ident "value2")))))
	(d-let
		(p-assign @23.1-23.7 (ident "value5"))
		(e-record @23.10-36.2
			(fields
				(field (name "a")
					(e-string @24.5-24.17
						(e-literal @24.8-24.17 (string "Multiline"))))
				(field (name "b")
					(e-tuple @26.5-31.3
						(elems
							(e-string @27.3-27.15
								(e-literal @27.6-27.15 (string "Multiline")))
							(e-string @29.3-29.15
								(e-literal @29.6-29.15 (string "Multiline"))))))
				(field (name "c")
					(e-list @32.5-35.3
						(elems
							(e-string @33.3-33.15
								(e-literal @33.6-33.15 (string "multiline")))))))))
	(d-let
		(p-assign @38.1-38.2 (ident "x"))
		(e-block @38.5-41.2
			(e-string @39.2-40.5
				(e-literal @40.2-40.5 (string "\n"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.7 (type "Str"))
		(patt @10.1-10.7 (type "Str"))
		(patt @13.1-13.7 (type "Str"))
		(patt @17.1-17.7 (type "Str"))
		(patt @23.1-23.7 (type "{ a: Str, b: (Str, Str), c: List(Str) }"))
		(patt @38.1-38.2 (type "Str")))
	(expressions
		(expr @8.10-8.50 (type "Str"))
		(expr @11.2-11.42 (type "Str"))
		(expr @13.10-15.14 (type "Str"))
		(expr @18.2-21.14 (type "Str"))
		(expr @23.10-36.2 (type "{ a: Str, b: (Str, Str), c: List(Str) }"))
		(expr @38.5-41.2 (type "Str"))))
~~~
