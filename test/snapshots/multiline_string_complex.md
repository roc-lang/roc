# META
~~~ini
description=multiline_string_complex
type=file
~~~
# SOURCE
~~~roc
module [value1, value2, value3, value4]

value1 = """This is a "string" with just one line"""

value2 = 
	"""This is a "string" with just one line"""

value3 = """This is a string
	"""With multiple lines
	"""${value1}

value4 = 
	"""This is a string
	# A comment in between
	"""With multiple lines
	"""${value2}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:15),Comma(1:15-1:16),LowerIdent(1:17-1:23),Comma(1:23-1:24),LowerIdent(1:25-1:31),Comma(1:31-1:32),LowerIdent(1:33-1:39),CloseSquare(1:39-1:40),
LowerIdent(3:1-3:7),OpAssign(3:8-3:9),MultilineStringStart(3:10-3:13),StringPart(3:13-3:50),MultilineStringEnd(3:50-3:53),
LowerIdent(5:1-5:7),OpAssign(5:8-5:9),
MultilineStringStart(6:2-6:5),StringPart(6:5-6:42),MultilineStringEnd(6:42-6:45),
LowerIdent(8:1-8:7),OpAssign(8:8-8:9),MultilineStringStart(8:10-8:13),StringPart(8:13-8:29),
MultilineStringStart(9:2-9:5),StringPart(9:5-9:24),
MultilineStringStart(10:2-10:5),StringPart(10:5-10:5),OpenStringInterpolation(10:5-10:7),LowerIdent(10:7-10:13),CloseStringInterpolation(10:13-10:14),StringPart(10:14-10:14),
LowerIdent(12:1-12:7),OpAssign(12:8-12:9),
MultilineStringStart(13:2-13:5),StringPart(13:5-13:21),
MultilineStringStart(15:2-15:5),StringPart(15:5-15:24),
MultilineStringStart(16:2-16:5),StringPart(16:5-16:5),OpenStringInterpolation(16:5-16:7),LowerIdent(16:7-16:13),CloseStringInterpolation(16:13-16:14),StringPart(16:14-16:14),EndOfFile(16:14-16:14),
~~~
# PARSE
~~~clojure
(file @1.1-16.14
	(module @1.1-1.40
		(exposes @1.8-1.40
			(exposed-lower-ident @1.9-1.15
				(text "value1"))
			(exposed-lower-ident @1.17-1.23
				(text "value2"))
			(exposed-lower-ident @1.25-1.31
				(text "value3"))
			(exposed-lower-ident @1.33-1.39
				(text "value4"))))
	(statements
		(s-decl @3.1-3.53
			(p-ident @3.1-3.7 (raw "value1"))
			(e-multiline-string @3.10-3.53
				(e-string-part @3.13-3.50 (raw "This is a "string" with just one line"))))
		(s-decl @5.1-6.45
			(p-ident @5.1-5.7 (raw "value2"))
			(e-multiline-string @6.2-6.45
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
				(e-string-part @16.14-16.14 (raw ""))))))
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
		(e-string @3.10-3.53
			(e-literal @3.13-3.50 (string "This is a "string" with just one line"))))
	(d-let
		(p-assign @5.1-5.7 (ident "value2"))
		(e-string @6.2-6.45
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
				(p-assign @5.1-5.7 (ident "value2"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.7 (type "Str"))
		(patt @5.1-5.7 (type "Str"))
		(patt @8.1-8.7 (type "Str"))
		(patt @12.1-12.7 (type "Str")))
	(expressions
		(expr @3.10-3.53 (type "Str"))
		(expr @6.2-6.45 (type "Str"))
		(expr @8.10-10.14 (type "Str"))
		(expr @13.2-16.14 (type "Str"))))
~~~
