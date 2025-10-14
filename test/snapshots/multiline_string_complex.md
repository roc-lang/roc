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
		x: \\Multiline
		,
	}

value1 = \\This is a "string" with just one line

value2 = 
	\\This is a "string" with just one line

value3 = \\This is a string
	\\With multiple lines
	\\${value1}

value4 = 
	\\This is a string
	# A comment in between
	\\With multiple lines
	\\${value2}

value5 = {
	a: \\Multiline
	,
	b: (
		\\Multiline
		,
		\\Multiline
		,
	),
	c: [
		\\multiline
		,
	],
	d: (
		0 - \\
		,
	),
	e: !\\
	,
}

x = {
	\\
	\\
}
~~~
# EXPECTED
TYPE MISMATCH - multiline_string_complex.md:37:7:37:9
TYPE MISMATCH - multiline_string_complex.md:40:6:40:8
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**multiline_string_complex.md:37:7:37:9:**
```roc
		0 - \\
```
		    ^^

It has the type:
    _Str_

But I expected it to be:
    _Num(_size)_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**multiline_string_complex.md:40:6:40:8:**
```roc
	e: !\\
```
	    ^^

It has the type:
    _Str_

But I expected it to be:
    _Bool_

# TOKENS
~~~zig
KwPackage(1:1-1:8),
OpenSquare(2:2-2:3),CloseSquare(2:3-2:4),
OpenCurly(3:2-3:3),
LowerIdent(4:3-4:4),OpColon(4:4-4:5),MultilineStringStart(4:6-4:8),StringPart(4:8-4:17),
Comma(5:3-5:4),
CloseCurly(6:2-6:3),
LowerIdent(8:1-8:7),OpAssign(8:8-8:9),MultilineStringStart(8:10-8:12),StringPart(8:12-8:49),
LowerIdent(10:1-10:7),OpAssign(10:8-10:9),
MultilineStringStart(11:2-11:4),StringPart(11:4-11:41),
LowerIdent(13:1-13:7),OpAssign(13:8-13:9),MultilineStringStart(13:10-13:12),StringPart(13:12-13:28),
MultilineStringStart(14:2-14:4),StringPart(14:4-14:23),
MultilineStringStart(15:2-15:4),StringPart(15:4-15:4),OpenStringInterpolation(15:4-15:6),LowerIdent(15:6-15:12),CloseStringInterpolation(15:12-15:13),StringPart(15:13-15:13),
LowerIdent(17:1-17:7),OpAssign(17:8-17:9),
MultilineStringStart(18:2-18:4),StringPart(18:4-18:20),
MultilineStringStart(20:2-20:4),StringPart(20:4-20:23),
MultilineStringStart(21:2-21:4),StringPart(21:4-21:4),OpenStringInterpolation(21:4-21:6),LowerIdent(21:6-21:12),CloseStringInterpolation(21:12-21:13),StringPart(21:13-21:13),
LowerIdent(23:1-23:7),OpAssign(23:8-23:9),OpenCurly(23:10-23:11),
LowerIdent(24:2-24:3),OpColon(24:3-24:4),MultilineStringStart(24:5-24:7),StringPart(24:7-24:16),
Comma(25:2-25:3),
LowerIdent(26:2-26:3),OpColon(26:3-26:4),OpenRound(26:5-26:6),
MultilineStringStart(27:3-27:5),StringPart(27:5-27:14),
Comma(28:3-28:4),
MultilineStringStart(29:3-29:5),StringPart(29:5-29:14),
Comma(30:3-30:4),
CloseRound(31:2-31:3),Comma(31:3-31:4),
LowerIdent(32:2-32:3),OpColon(32:3-32:4),OpenSquare(32:5-32:6),
MultilineStringStart(33:3-33:5),StringPart(33:5-33:14),
Comma(34:3-34:4),
CloseSquare(35:2-35:3),Comma(35:3-35:4),
LowerIdent(36:2-36:3),OpColon(36:3-36:4),OpenRound(36:5-36:6),
Int(37:3-37:4),OpBinaryMinus(37:5-37:6),MultilineStringStart(37:7-37:9),StringPart(37:9-37:9),
Comma(38:3-38:4),
CloseRound(39:2-39:3),Comma(39:3-39:4),
LowerIdent(40:2-40:3),OpColon(40:3-40:4),OpBang(40:5-40:6),MultilineStringStart(40:6-40:8),StringPart(40:8-40:8),
Comma(41:2-41:3),
CloseCurly(42:1-42:2),
LowerIdent(44:1-44:2),OpAssign(44:3-44:4),OpenCurly(44:5-44:6),
MultilineStringStart(45:2-45:4),StringPart(45:4-45:4),
MultilineStringStart(46:2-46:4),StringPart(46:4-46:4),
CloseCurly(47:1-47:2),
EndOfFile(48:1-48:1),
~~~
# PARSE
~~~clojure
(file @1.1-47.2
	(package @1.1-6.3
		(exposes @2.2-2.4)
		(packages @3.2-6.3
			(record-field @4.3-4.17 (name "x")
				(e-multiline-string @4.6-4.17
					(e-string-part @4.8-4.17 (raw "Multiline"))))))
	(statements
		(s-decl @8.1-8.49
			(p-ident @8.1-8.7 (raw "value1"))
			(e-multiline-string @8.10-8.49
				(e-string-part @8.12-8.49 (raw "This is a "string" with just one line"))))
		(s-decl @10.1-11.41
			(p-ident @10.1-10.7 (raw "value2"))
			(e-multiline-string @11.2-11.41
				(e-string-part @11.4-11.41 (raw "This is a "string" with just one line"))))
		(s-decl @13.1-15.13
			(p-ident @13.1-13.7 (raw "value3"))
			(e-multiline-string @13.10-15.13
				(e-string-part @13.12-13.28 (raw "This is a string"))
				(e-string-part @14.4-14.23 (raw "With multiple lines"))
				(e-string-part @15.4-15.4 (raw ""))
				(e-ident @15.6-15.12 (raw "value1"))
				(e-string-part @15.13-15.13 (raw ""))))
		(s-decl @17.1-21.13
			(p-ident @17.1-17.7 (raw "value4"))
			(e-multiline-string @18.2-21.13
				(e-string-part @18.4-18.20 (raw "This is a string"))
				(e-string-part @20.4-20.23 (raw "With multiple lines"))
				(e-string-part @21.4-21.4 (raw ""))
				(e-ident @21.6-21.12 (raw "value2"))
				(e-string-part @21.13-21.13 (raw ""))))
		(s-decl @23.1-42.2
			(p-ident @23.1-23.7 (raw "value5"))
			(e-record @23.10-42.2
				(field (field "a")
					(e-multiline-string @24.5-24.16
						(e-string-part @24.7-24.16 (raw "Multiline"))))
				(field (field "b")
					(e-tuple @26.5-31.3
						(e-multiline-string @27.3-27.14
							(e-string-part @27.5-27.14 (raw "Multiline")))
						(e-multiline-string @29.3-29.14
							(e-string-part @29.5-29.14 (raw "Multiline")))))
				(field (field "c")
					(e-list @32.5-35.3
						(e-multiline-string @33.3-33.14
							(e-string-part @33.5-33.14 (raw "multiline")))))
				(field (field "d")
					(e-tuple @36.5-39.3
						(e-binop @37.3-37.9 (op "-")
							(e-int @37.3-37.4 (raw "0"))
							(e-multiline-string @37.7-37.9
								(e-string-part @37.9-37.9 (raw ""))))))
				(field (field "e")
					(unary "!"
						(e-multiline-string @40.6-40.8
							(e-string-part @40.8-40.8 (raw "")))))))
		(s-decl @44.1-47.2
			(p-ident @44.1-44.2 (raw "x"))
			(e-block @44.5-47.2
				(statements
					(e-multiline-string @45.2-46.4
						(e-string-part @45.4-45.4 (raw ""))
						(e-string-part @46.4-46.4 (raw ""))))))))
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
		(e-string @8.10-8.49
			(e-literal @8.12-8.49 (string "This is a "string" with just one line"))))
	(d-let
		(p-assign @10.1-10.7 (ident "value2"))
		(e-string @11.2-11.41
			(e-literal @11.4-11.41 (string "This is a "string" with just one line"))))
	(d-let
		(p-assign @13.1-13.7 (ident "value3"))
		(e-string @13.10-15.13
			(e-literal @13.12-13.28 (string "This is a string"))
			(e-literal @14.2-14.4 (string "\n"))
			(e-literal @14.4-14.23 (string "With multiple lines"))
			(e-literal @15.2-15.4 (string "\n"))
			(e-lookup-local @15.6-15.12
				(p-assign @8.1-8.7 (ident "value1")))))
	(d-let
		(p-assign @17.1-17.7 (ident "value4"))
		(e-string @18.2-21.13
			(e-literal @18.4-18.20 (string "This is a string"))
			(e-literal @20.2-20.4 (string "\n"))
			(e-literal @20.4-20.23 (string "With multiple lines"))
			(e-literal @21.2-21.4 (string "\n"))
			(e-lookup-local @21.6-21.12
				(p-assign @10.1-10.7 (ident "value2")))))
	(d-let
		(p-assign @23.1-23.7 (ident "value5"))
		(e-record @23.10-42.2
			(fields
				(field (name "a")
					(e-string @24.5-24.16
						(e-literal @24.7-24.16 (string "Multiline"))))
				(field (name "b")
					(e-tuple @26.5-31.3
						(elems
							(e-string @27.3-27.14
								(e-literal @27.5-27.14 (string "Multiline")))
							(e-string @29.3-29.14
								(e-literal @29.5-29.14 (string "Multiline"))))))
				(field (name "c")
					(e-list @32.5-35.3
						(elems
							(e-string @33.3-33.14
								(e-literal @33.5-33.14 (string "multiline"))))))
				(field (name "d")
					(e-binop @37.3-37.9 (op "sub")
						(e-num @37.3-37.4 (value "0"))
						(e-string @37.7-37.9)))
				(field (name "e")
					(e-unary-not @40.5-40.8
						(e-string @40.6-40.8))))))
	(d-let
		(p-assign @44.1-44.2 (ident "x"))
		(e-block @44.5-47.2
			(e-string @45.2-46.4
				(e-literal @46.2-46.4 (string "\n"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.7 (type "Str"))
		(patt @10.1-10.7 (type "Str"))
		(patt @13.1-13.7 (type "Str"))
		(patt @17.1-17.7 (type "Str"))
		(patt @23.1-23.7 (type "{ a: Str, b: (Str, Str), c: List(Str), d: Error, e: Error }"))
		(patt @44.1-44.2 (type "Str")))
	(expressions
		(expr @8.10-8.49 (type "Str"))
		(expr @11.2-11.41 (type "Str"))
		(expr @13.10-15.13 (type "Str"))
		(expr @18.2-21.13 (type "Str"))
		(expr @23.10-42.2 (type "{ a: Str, b: (Str, Str), c: List(Str), d: Error, e: Error }"))
		(expr @44.5-47.2 (type "Str"))))
~~~
