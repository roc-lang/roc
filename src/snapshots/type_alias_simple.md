# META
~~~ini
description=Simple type alias usage in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

UserId : U64

getUser : UserId -> Str
getUser = |id| if (id > 10) "big" else "small"

main! = |_| getUser(100)
~~~
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_alias_simple.md:6:19:6:28:**
```roc
getUser = |id| if (id > 10) "big" else "small"
```
                  ^^^^^^^^^

It is of type:
    _(*)_

But you are trying to use it as:
    _[True, False]_

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:7),OpColon(3:8-3:9),UpperIdent(3:10-3:13),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:8),OpColon(5:9-5:10),UpperIdent(5:11-5:17),OpArrow(5:18-5:20),UpperIdent(5:21-5:24),Newline(1:1-1:1),
LowerIdent(6:1-6:8),OpAssign(6:9-6:10),OpBar(6:11-6:12),LowerIdent(6:12-6:14),OpBar(6:14-6:15),KwIf(6:16-6:18),OpenRound(6:19-6:20),LowerIdent(6:20-6:22),OpGreaterThan(6:23-6:24),Int(6:25-6:27),CloseRound(6:27-6:28),StringStart(6:29-6:30),StringPart(6:30-6:33),StringEnd(6:33-6:34),KwElse(6:35-6:39),StringStart(6:40-6:41),StringPart(6:41-6:46),StringEnd(6:46-6:47),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(8:1-8:6),OpAssign(8:7-8:8),OpBar(8:9-8:10),Underscore(8:10-8:11),OpBar(8:11-8:12),LowerIdent(8:13-8:20),NoSpaceOpenRound(8:20-8:21),Int(8:21-8:24),CloseRound(8:24-8:25),EndOfFile(8:25-8:25),
~~~
# PARSE
~~~clojure
(file @1.1-8.25
	(app @1.1-1.53
		(provides @1.6-1.12
			(exposed-lower-ident (text "main!")))
		(record-field @1.15-1.53 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.53 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-decl @3.1-5.8
			(header @3.1-3.7 (name "UserId")
				(args))
			(ty (name "U64")))
		(s-type-anno @5.1-6.8 (name "getUser")
			(ty-fn @5.11-5.24
				(ty (name "UserId"))
				(ty (name "Str"))))
		(s-decl @1.1-1.1
			(p-ident @6.1-6.8 (raw "getUser"))
			(e-lambda @1.1-1.1
				(args
					(p-ident @6.12-6.14 (raw "id")))
				(e-if-then-else @1.1-1.1
					(e-tuple @6.19-6.28
						(e-binop @6.20-6.28 (op ">")
							(e-ident @6.20-6.22 (qaul "") (raw "id"))
							(e-int @6.25-6.27 (raw "10"))))
					(e-string @6.29-6.34
						(e-string-part @6.30-6.33 (raw "big")))
					(e-string @6.40-6.47
						(e-string-part @6.41-6.46 (raw "small"))))))
		(s-decl @8.1-8.25
			(p-ident @8.1-8.6 (raw "main!"))
			(e-lambda @8.9-8.25
				(args
					(p-underscore))
				(e-apply @8.13-8.25
					(e-ident @8.13-8.20 (qaul "") (raw "getUser"))
					(e-int @8.21-8.24 (raw "100")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 99)
		(p-assign @6.1-6.8 (ident "getUser") (id 79))
		(e-lambda @1.1-1.1 (id 92)
			(args
				(p-assign @6.12-6.14 (ident "id") (id 80)))
			(e-if @1.1-1.1
				(if-branches
					(if-branch
						(e-tuple @6.19-6.28
							(elems
								(e-binop @6.20-6.28 (op "gt")
									(e-lookup-local @6.20-6.22
										(pattern (id 80)))
									(e-int @6.25-6.27 (value "10")))))
						(e-string @6.29-6.34
							(e-literal @6.30-6.33 (string "big")))))
				(if-else
					(e-string @6.40-6.47
						(e-literal @6.41-6.46 (string "small"))))))
		(annotation @6.1-6.8 (signature 97) (id 98)
			(declared-type
				(ty-fn @5.11-5.24 (effectful false)
					(ty @5.11-5.17 (name "UserId"))
					(ty @5.21-5.24 (name "Str"))))))
	(d-let (id 108)
		(p-assign @8.1-8.6 (ident "main!") (id 100))
		(e-lambda @8.9-8.25 (id 107)
			(args
				(p-underscore @8.10-8.11 (id 101)))
			(e-call @8.13-8.25
				(e-lookup-local @8.13-8.20
					(pattern (id 79)))
				(e-int @8.21-8.24 (value "100")))))
	(s-type-decl @3.1-5.8 (id 75)
		(ty-header @3.1-3.7 (name "UserId"))
		(ty @3.10-3.13 (name "U64"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "getUser") (def_var 99) (type "UserId -> Str"))
		(d_assign (name "main!") (def_var 108) (type "* ? *")))
	(expressions
		(expr @1.1-1.1 (type "UserId -> Str"))
		(expr @8.9-8.25 (type "* ? *"))))
~~~
