# META
~~~ini
description=Basic tag union type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

process : [Some(Str), None] -> Str
process = |maybe| "result"

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable ``maybe`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_maybe` to suppress this warning.
The unused variable is declared here:
**type_tag_union_basic.md:4:12:4:17:**
```roc
process = |maybe| "result"
```
           ^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),OpenSquare(3:11-3:12),UpperIdent(3:12-3:16),NoSpaceOpenRound(3:16-3:17),UpperIdent(3:17-3:20),CloseRound(3:20-3:21),Comma(3:21-3:22),UpperIdent(3:23-3:27),CloseSquare(3:27-3:28),OpArrow(3:29-3:31),UpperIdent(3:32-3:35),Newline(1:1-1:1),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),LowerIdent(4:12-4:17),OpBar(4:17-4:18),StringStart(4:19-4:20),StringPart(4:20-4:26),StringEnd(4:26-4:27),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),OpenCurly(6:13-6:14),CloseCurly(6:14-6:15),EndOfFile(6:15-6:15),
~~~
# PARSE
~~~clojure
(file @1.1-6.15
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
		(s-type-anno @1.1-1.1 (name "process")
			(ty-fn @3.11-3.35
				(ty-tag-union @3.11-3.28
					(tags
						(ty-apply @3.12-3.21
							(ty @3.12-3.16 (name "Some"))
							(ty @3.17-3.20 (name "Str")))
						(ty @3.23-3.27 (name "None"))))
				(ty @3.32-3.35 (name "Str"))))
		(s-decl @4.1-4.27
			(p-ident @4.1-4.8 (raw "process"))
			(e-lambda @4.11-4.27
				(args
					(p-ident @4.12-4.17 (raw "maybe")))
				(e-string @4.19-4.27
					(e-string-part @4.20-4.26 (raw "result")))))
		(s-decl @6.1-6.15
			(p-ident @6.1-6.6 (raw "main!"))
			(e-lambda @6.9-6.15
				(args
					(p-underscore))
				(e-record @6.13-6.15)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "process"))
		(e-lambda @4.11-4.27
			(args
				(p-assign @4.12-4.17 (ident "maybe")))
			(e-string @4.19-4.27
				(e-literal @4.20-4.26 (string "result"))))
		(annotation @4.1-4.8
			(declared-type
				(ty-fn @3.11-3.35 (effectful false)
					(ty-tag-union @3.11-3.28
						(ty-apply @3.12-3.21 (symbol "Some")
							(ty @3.17-3.20 (name "Str")))
						(ty @3.23-3.27 (name "None")))
					(ty @3.32-3.35 (name "Str"))))))
	(d-let
		(p-assign @6.1-6.6 (ident "main!"))
		(e-lambda @6.9-6.15
			(args
				(p-underscore @6.10-6.11))
			(e-empty_record @6.13-6.15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.8 (type "* -> Str"))
		(patt @6.1-6.6 (type "* -> {}")))
	(expressions
		(expr @4.11-4.27 (type "* -> Str"))
		(expr @6.9-6.15 (type "* -> {}"))))
~~~
