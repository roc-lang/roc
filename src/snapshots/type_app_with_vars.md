# META
~~~ini
description=Type application with variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

mapList : List(a), (a -> b) -> List(b)
mapList = |list, fn| list.map(fn)

main! = |_| mapList([1,2,3,4,5])
~~~
# EXPECTED
TYPE MISMATCH - type_app_with_vars.md:6:13:6:20
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_app_with_vars.md:6:13:6:20:**
```roc
main! = |_| mapList([1,2,3,4,5])
```
            ^^^^^^^

It is of type:
    _Error, * -> * -> Error_

But you are trying to use it as:
    _List(Num(*)) -> *_

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),UpperIdent(3:11-3:15),NoSpaceOpenRound(3:15-3:16),LowerIdent(3:16-3:17),CloseRound(3:17-3:18),Comma(3:18-3:19),OpenRound(3:20-3:21),LowerIdent(3:21-3:22),OpArrow(3:23-3:25),LowerIdent(3:26-3:27),CloseRound(3:27-3:28),OpArrow(3:29-3:31),UpperIdent(3:32-3:36),NoSpaceOpenRound(3:36-3:37),LowerIdent(3:37-3:38),CloseRound(3:38-3:39),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),LowerIdent(4:12-4:16),Comma(4:16-4:17),LowerIdent(4:18-4:20),OpBar(4:20-4:21),LowerIdent(4:22-4:26),NoSpaceDotLowerIdent(4:26-4:30),NoSpaceOpenRound(4:30-4:31),LowerIdent(4:31-4:33),CloseRound(4:33-4:34),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),LowerIdent(6:13-6:20),NoSpaceOpenRound(6:20-6:21),OpenSquare(6:21-6:22),Int(6:22-6:23),Comma(6:23-6:24),Int(6:24-6:25),Comma(6:25-6:26),Int(6:26-6:27),Comma(6:27-6:28),Int(6:28-6:29),Comma(6:29-6:30),Int(6:30-6:31),CloseSquare(6:31-6:32),CloseRound(6:32-6:33),EndOfFile(6:33-6:33),
~~~
# PARSE
~~~clojure
(file @1.1-6.33
	(app @1.1-1.53
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11 (text "main!")))
		(record-field @1.15-1.51 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.51 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @3.1-3.39 (name "mapList")
			(ty-fn @3.11-3.39
				(ty-apply @3.11-3.18
					(ty @3.11-3.15 (name "List"))
					(ty-var @3.16-3.16 (raw "a")))
				(ty-fn @3.21-3.27
					(ty-var @3.21-3.21 (raw "a"))
					(ty-var @1.1-1.1 (raw "b")))
				(ty-apply @3.32-3.39
					(ty @3.32-3.36 (name "List"))
					(ty-var @3.37-3.37 (raw "b")))))
		(s-decl @4.1-4.34
			(p-ident @4.1-4.8 (raw "mapList"))
			(e-lambda @4.11-4.34
				(args
					(p-ident @4.12-4.16 (raw "list"))
					(p-ident @4.18-4.20 (raw "fn")))
				(e-field-access @4.22-4.34
					(e-ident @4.22-4.26 (raw "list"))
					(e-apply @4.26-4.34
						(e-ident @4.26-4.30 (raw "map"))
						(e-ident @4.31-4.33 (raw "fn"))))))
		(s-decl @6.1-6.33
			(p-ident @6.1-6.6 (raw "main!"))
			(e-lambda @6.9-6.33
				(args
					(p-underscore))
				(e-apply @6.13-6.33
					(e-ident @6.13-6.20 (raw "mapList"))
					(e-list @6.21-6.32
						(e-int @6.22-6.23 (raw "1"))
						(e-int @6.24-6.25 (raw "2"))
						(e-int @6.26-6.27 (raw "3"))
						(e-int @6.28-6.29 (raw "4"))
						(e-int @6.30-6.31 (raw "5"))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

mapList : List(a), (a -> b) -> List(b)
mapList = |list, fn| list.map(fn)

main! = |_| mapList([1, 2, 3, 4, 5])
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "mapList"))
		(e-lambda @4.11-4.34
			(args
				(p-assign @4.12-4.16 (ident "list"))
				(p-assign @4.18-4.20 (ident "fn")))
			(e-dot-access @4.22-4.34 (field "map")
				(receiver
					(e-lookup-local @4.22-4.26
						(p-assign @4.12-4.16 (ident "list"))))
				(args
					(e-lookup-local @4.31-4.33
						(p-assign @4.18-4.20 (ident "fn"))))))
		(annotation @4.1-4.8
			(declared-type
				(ty-fn @3.11-3.39 (effectful false)
					(ty-apply @3.11-3.18 (symbol "List")
						(ty-var @3.16-3.16 (name "a")))
					(ty-parens @3.20-3.28
						(ty-fn @3.21-3.27 (effectful false)
							(ty-var @3.21-3.21 (name "a"))
							(ty-var @1.1-1.1 (name "b"))))
					(ty-apply @3.32-3.39 (symbol "List")
						(ty-var @3.37-3.37 (name "b")))))))
	(d-let
		(p-assign @6.1-6.6 (ident "main!"))
		(e-lambda @6.9-6.33
			(args
				(p-underscore @6.10-6.11))
			(e-call @6.13-6.33
				(e-lookup-local @6.13-6.20
					(p-assign @4.1-4.8 (ident "mapList")))
				(e-list @6.21-6.32
					(elems
						(e-int @6.22-6.23 (value "1"))
						(e-int @6.24-6.25 (value "2"))
						(e-int @6.26-6.27 (value "3"))
						(e-int @6.28-6.29 (value "4"))
						(e-int @6.30-6.31 (value "5"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.8 (type "Error"))
		(patt @6.1-6.6 (type "* -> *")))
	(expressions
		(expr @4.11-4.34 (type "Error"))
		(expr @6.9-6.33 (type "* -> *"))))
~~~
