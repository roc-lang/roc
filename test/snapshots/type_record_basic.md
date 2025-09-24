# META
~~~ini
description=Basic record type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getName : { name: Str, age: U64 } -> Str
getName = |_person| "hello"

main! = |_| getName({namee: "luke", age:21})
~~~
# EXPECTED
TYPE MISMATCH - type_record_basic.md:6:21:6:44
# PROBLEMS
**TYPE MISMATCH**
The first argument being passed to this function has the wrong type:
**type_record_basic.md:6:21:6:44:**
```roc
main! = |_| getName({namee: "luke", age:21})
```
                    ^^^^^^^^^^^^^^^^^^^^^^^

This argument has the type:
    _{ age: Num(_size), namee: Str }_

But `getName` needs the first argument to be:
    _{ age: Num(Int(Unsigned64)), name: Str }_

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),OpenCurly(3:11-3:12),LowerIdent(3:13-3:17),OpColon(3:17-3:18),UpperIdent(3:19-3:22),Comma(3:22-3:23),LowerIdent(3:24-3:27),OpColon(3:27-3:28),UpperIdent(3:29-3:32),CloseCurly(3:33-3:34),OpArrow(3:35-3:37),UpperIdent(3:38-3:41),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),NamedUnderscore(4:12-4:19),OpBar(4:19-4:20),StringStart(4:21-4:22),StringPart(4:22-4:27),StringEnd(4:27-4:28),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),LowerIdent(6:13-6:20),NoSpaceOpenRound(6:20-6:21),OpenCurly(6:21-6:22),LowerIdent(6:22-6:27),OpColon(6:27-6:28),StringStart(6:29-6:30),StringPart(6:30-6:34),StringEnd(6:34-6:35),Comma(6:35-6:36),LowerIdent(6:37-6:40),OpColon(6:40-6:41),Int(6:41-6:43),CloseCurly(6:43-6:44),CloseRound(6:44-6:45),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.45
	(app @1.1-1.53
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.51 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.51 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @3.1-3.41 (name "getName")
			(ty-fn @3.11-3.41
				(ty-record @3.11-3.34
					(anno-record-field @3.13-3.22 (name "name")
						(ty @3.19-3.22 (name "Str")))
					(anno-record-field @3.24-3.32 (name "age")
						(ty @3.29-3.32 (name "U64"))))
				(ty @3.38-3.41 (name "Str"))))
		(s-decl @4.1-4.28
			(p-ident @4.1-4.8 (raw "getName"))
			(e-lambda @4.11-4.28
				(args
					(p-ident @4.12-4.19 (raw "_person")))
				(e-string @4.21-4.28
					(e-string-part @4.22-4.27 (raw "hello")))))
		(s-decl @6.1-6.45
			(p-ident @6.1-6.6 (raw "main!"))
			(e-lambda @6.9-6.45
				(args
					(p-underscore))
				(e-apply @6.13-6.45
					(e-ident @6.13-6.20 (raw "getName"))
					(e-record @6.21-6.44
						(field (field "namee")
							(e-string @6.29-6.35
								(e-string-part @6.30-6.34 (raw "luke"))))
						(field (field "age")
							(e-int @6.41-6.43 (raw "21")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getName : { name : Str, age : U64 } -> Str
getName = |_person| "hello"

main! = |_| getName({ namee: "luke", age: 21 })
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "getName"))
		(e-lambda @4.11-4.28
			(args
				(p-assign @4.12-4.19 (ident "_person")))
			(e-string @4.21-4.28
				(e-literal @4.22-4.27 (string "hello"))))
		(annotation @4.1-4.8
			(declared-type
				(ty-fn @3.11-3.41 (effectful false)
					(ty-record @3.11-3.34
						(field (field "name")
							(ty-lookup @3.19-3.22 (name "Str") (builtin)))
						(field (field "age")
							(ty-lookup @3.29-3.32 (name "U64") (builtin))))
					(ty-lookup @3.38-3.41 (name "Str") (builtin))))))
	(d-let
		(p-assign @6.1-6.6 (ident "main!"))
		(e-closure @6.9-6.45
			(captures
				(capture @4.1-4.8 (ident "getName")))
			(e-lambda @6.9-6.45
				(args
					(p-underscore @6.10-6.11))
				(e-call @6.13-6.45
					(e-record @6.21-6.44
						(fields
							(field (name "namee")
								(e-string @6.29-6.35
									(e-literal @6.30-6.34 (string "luke"))))
							(field (name "age")
								(e-num @6.41-6.43 (value "21"))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.8 (type "{ age: Num(Int(Unsigned64)), name: Str } -> Str"))
		(patt @6.1-6.6 (type "_arg -> Error")))
	(expressions
		(expr @4.11-4.28 (type "{ age: Num(Int(Unsigned64)), name: Str } -> Str"))
		(expr @6.9-6.45 (type "_arg -> Error"))))
~~~
