# META
~~~ini
description=Type alias with tag union and type parameters
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Type alias with type parameters that expands to a tag union
MyResult(ok, err) : [Good(ok), Bad(err)]

# Using the type alias
process : MyResult(Str, I32) -> Str
process = |_result| "processed"

# Another type alias with a single parameter
Option(a) : [Some(a), None]

# Using it with different types
getString : Option(Str) -> Str
getString = |_opt| "default"

getNumber : Option(I32) -> I32
getNumber = |_opt| 0

main! = |_| {}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:62),
UpperIdent(4:1-4:9),NoSpaceOpenRound(4:9-4:10),LowerIdent(4:10-4:12),Comma(4:12-4:13),LowerIdent(4:14-4:17),CloseRound(4:17-4:18),OpColon(4:19-4:20),OpenSquare(4:21-4:22),UpperIdent(4:22-4:26),NoSpaceOpenRound(4:26-4:27),LowerIdent(4:27-4:29),CloseRound(4:29-4:30),Comma(4:30-4:31),UpperIdent(4:32-4:35),NoSpaceOpenRound(4:35-4:36),LowerIdent(4:36-4:39),CloseRound(4:39-4:40),CloseSquare(4:40-4:41),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:2-6:23),
LowerIdent(7:1-7:8),OpColon(7:9-7:10),UpperIdent(7:11-7:19),NoSpaceOpenRound(7:19-7:20),UpperIdent(7:20-7:23),Comma(7:23-7:24),UpperIdent(7:25-7:28),CloseRound(7:28-7:29),OpArrow(7:30-7:32),UpperIdent(7:33-7:36),Newline(1:1-1:1),
LowerIdent(8:1-8:8),OpAssign(8:9-8:10),OpBar(8:11-8:12),NamedUnderscore(8:12-8:19),OpBar(8:19-8:20),StringStart(8:21-8:22),StringPart(8:22-8:31),StringEnd(8:31-8:32),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(10:2-10:45),
UpperIdent(11:1-11:7),NoSpaceOpenRound(11:7-11:8),LowerIdent(11:8-11:9),CloseRound(11:9-11:10),OpColon(11:11-11:12),OpenSquare(11:13-11:14),UpperIdent(11:14-11:18),NoSpaceOpenRound(11:18-11:19),LowerIdent(11:19-11:20),CloseRound(11:20-11:21),Comma(11:21-11:22),UpperIdent(11:23-11:27),CloseSquare(11:27-11:28),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(13:2-13:32),
LowerIdent(14:1-14:10),OpColon(14:11-14:12),UpperIdent(14:13-14:19),NoSpaceOpenRound(14:19-14:20),UpperIdent(14:20-14:23),CloseRound(14:23-14:24),OpArrow(14:25-14:27),UpperIdent(14:28-14:31),Newline(1:1-1:1),
LowerIdent(15:1-15:10),OpAssign(15:11-15:12),OpBar(15:13-15:14),NamedUnderscore(15:14-15:18),OpBar(15:18-15:19),StringStart(15:20-15:21),StringPart(15:21-15:28),StringEnd(15:28-15:29),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(17:1-17:10),OpColon(17:11-17:12),UpperIdent(17:13-17:19),NoSpaceOpenRound(17:19-17:20),UpperIdent(17:20-17:23),CloseRound(17:23-17:24),OpArrow(17:25-17:27),UpperIdent(17:28-17:31),Newline(1:1-1:1),
LowerIdent(18:1-18:10),OpAssign(18:11-18:12),OpBar(18:13-18:14),NamedUnderscore(18:14-18:18),OpBar(18:18-18:19),Int(18:20-18:21),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(20:1-20:6),OpAssign(20:7-20:8),OpBar(20:9-20:10),Underscore(20:10-20:11),OpBar(20:11-20:12),OpenCurly(20:13-20:14),CloseCurly(20:14-20:15),EndOfFile(20:15-20:15),
~~~
# PARSE
~~~clojure
(file @1-1-20-15
	(app @1-1-1-53
		(provides @1-6-1-12
			(exposed-lower-ident (text "main!")))
		(record-field @1-15-1-53 (name "pf")
			(e-string @1-28-1-51
				(e-string-part @1-29-1-50 (raw "../basic-cli/main.roc"))))
		(packages @1-13-1-53
			(record-field @1-15-1-53 (name "pf")
				(e-string @1-28-1-51
					(e-string-part @1-29-1-50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-decl @4-1-7-8
			(header @4-1-4-18 (name "MyResult")
				(args
					(ty-var @4-10-4-12 (raw "ok"))
					(ty-var @4-14-4-17 (raw "err"))))
			(ty-tag-union @4-21-4-41
				(tags
					(ty-apply @4-22-4-30
						(ty (name "Good"))
						(ty-var @4-27-4-29 (raw "ok")))
					(ty-apply @4-32-4-40
						(ty (name "Bad"))
						(ty-var @4-36-4-39 (raw "err"))))))
		(s-type-anno @7-1-8-8 (name "process")
			(ty-fn @7-11-7-36
				(ty-apply @7-11-7-29
					(ty (name "MyResult"))
					(ty (name "Str"))
					(ty (name "I32")))
				(ty (name "Str"))))
		(s-decl @8-1-8-32
			(p-ident @8-1-8-8 (raw "process"))
			(e-lambda @8-11-8-32
				(args
					(p-ident @8-12-8-19 (raw "_result")))
				(e-string @8-21-8-32
					(e-string-part @8-22-8-31 (raw "processed")))))
		(s-type-decl @11-1-14-10
			(header @11-1-11-10 (name "Option")
				(args
					(ty-var @11-8-11-9 (raw "a"))))
			(ty-tag-union @11-13-11-28
				(tags
					(ty-apply @11-14-11-21
						(ty (name "Some"))
						(ty-var @11-19-11-20 (raw "a")))
					(ty (name "None")))))
		(s-type-anno @14-1-15-10 (name "getString")
			(ty-fn @14-13-14-31
				(ty-apply @14-13-14-24
					(ty (name "Option"))
					(ty (name "Str")))
				(ty (name "Str"))))
		(s-decl @15-1-15-29
			(p-ident @15-1-15-10 (raw "getString"))
			(e-lambda @15-13-15-29
				(args
					(p-ident @15-14-15-18 (raw "_opt")))
				(e-string @15-20-15-29
					(e-string-part @15-21-15-28 (raw "default")))))
		(s-type-anno @17-1-18-10 (name "getNumber")
			(ty-fn @17-13-17-31
				(ty-apply @17-13-17-24
					(ty (name "Option"))
					(ty (name "I32")))
				(ty (name "I32"))))
		(s-decl @18-1-18-21
			(p-ident @18-1-18-10 (raw "getNumber"))
			(e-lambda @18-13-18-21
				(args
					(p-ident @18-14-18-18 (raw "_opt")))
				(e-int @18-20-18-21 (raw "0"))))
		(s-decl @20-1-20-15
			(p-ident @20-1-20-6 (raw "main!"))
			(e-lambda @20-9-20-15
				(args
					(p-underscore))
				(e-record @20-13-20-15)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 103)
		(p-assign @8-1-8-8 (ident "process") (id 93))
		(e-lambda @8-11-8-32 (id 97)
			(args
				(p-assign @8-12-8-19 (ident "_result") (id 94)))
			(e-string @8-21-8-32
				(e-literal @8-22-8-31 (string "processed"))))
		(annotation @8-1-8-8 (signature 101) (id 102)
			(declared-type
				(ty-fn @7-11-7-36 (effectful false)
					(ty-apply @7-11-7-29 (symbol "MyResult")
						(ty @7-20-7-23 (name "Str"))
						(ty @7-25-7-28 (name "I32")))
					(ty @7-33-7-36 (name "Str"))))))
	(d-let (id 118)
		(p-assign @15-1-15-10 (ident "getString") (id 108))
		(e-lambda @15-13-15-29 (id 112)
			(args
				(p-assign @15-14-15-18 (ident "_opt") (id 109)))
			(e-string @15-20-15-29
				(e-literal @15-21-15-28 (string "default"))))
		(annotation @15-1-15-10 (signature 116) (id 117)
			(declared-type
				(ty-fn @14-13-14-31 (effectful false)
					(ty-apply @14-13-14-24 (symbol "Option")
						(ty @14-20-14-23 (name "Str")))
					(ty @14-28-14-31 (name "Str"))))))
	(d-let (id 133)
		(p-assign @18-1-18-10 (ident "getNumber") (id 123))
		(e-lambda @18-13-18-21 (id 127)
			(args
				(p-assign @18-14-18-18 (ident "_opt") (id 124)))
			(e-int @18-20-18-21 (num-var 126) (value "0")))
		(annotation @18-1-18-10 (signature 131) (id 132)
			(declared-type
				(ty-fn @17-13-17-31 (effectful false)
					(ty-apply @17-13-17-24 (symbol "Option")
						(ty @17-20-17-23 (name "I32")))
					(ty @17-28-17-31 (name "I32"))))))
	(d-let (id 138)
		(p-assign @20-1-20-6 (ident "main!") (id 134))
		(e-lambda @20-9-20-15 (id 137)
			(args
				(p-underscore @20-10-20-11 (id 135)))
			(e-empty_record @20-13-20-15)))
	(s-type-decl @4-1-7-8 (id 80)
		(ty-header @4-1-4-18 (name "MyResult")
			(ty-args
				(ty-var @4-10-4-12 (name "ok"))
				(ty-var @4-14-4-17 (name "err"))))
		(ty-tag-union @4-21-4-41
			(ty-apply @4-22-4-30 (symbol "Good")
				(ty-var @4-27-4-29 (name "ok")))
			(ty-apply @4-32-4-40 (symbol "Bad")
				(ty-var @4-36-4-39 (name "err")))))
	(s-type-decl @11-1-14-10 (id 87)
		(ty-header @11-1-11-10 (name "Option")
			(ty-args
				(ty-var @11-8-11-9 (name "a"))))
		(ty-tag-union @11-13-11-28
			(ty-apply @11-14-11-21 (symbol "Some")
				(ty-var @11-19-11-20 (name "a")))
			(ty @11-23-11-27 (name "None")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "process") (type "*"))
		(def (name "getString") (type "*"))
		(def (name "getNumber") (type "*"))
		(def (name "main!") (type "*")))
	(expressions
		(expr @8-11-8-32 (type "*"))
		(expr @15-13-15-29 (type "*"))
		(expr @18-13-18-21 (type "*"))
		(expr @20-9-20-15 (type "*"))))
~~~