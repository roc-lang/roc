# META
~~~ini
description=Two decls
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

a = 5
b = a + 1
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Int(3:5-3:6),Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),LowerIdent(4:5-4:6),OpPlus(4:7-4:8),Int(4:9-4:10),EndOfFile(4:10-4:10),
~~~
# PARSE
~~~clojure
(file @1-1-4-10
	(app @1-1-1-57
		(provides @1-6-1-12
			(exposed-lower-ident (text "main!")))
		(record-field @1-15-1-57 (name "pf")
			(e-string @1-28-1-55
				(e-string-part @1-29-1-54 (raw "../basic-cli/platform.roc"))))
		(packages @1-13-1-57
			(record-field @1-15-1-57 (name "pf")
				(e-string @1-28-1-55
					(e-string-part @1-29-1-54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-decl @3-1-3-6
			(p-ident @3-1-3-2 (raw "a"))
			(e-int @3-5-3-6 (raw "5")))
		(s-decl @4-1-4-10
			(p-ident @4-1-4-2 (raw "b"))
			(e-binop @4-5-4-10 (op "+")
				(e-ident @4-5-4-6 (qaul "") (raw "a"))
				(e-int @4-9-4-10 (raw "1"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 75)
		(p-assign @3-1-3-2 (ident "a") (id 72))
		(e-int @3-5-3-6 (num-var 74) (value "5") (id 74)))
	(d-let (id 81)
		(p-assign @4-1-4-2 (ident "b") (id 76))
		(e-binop @4-5-4-10 (op "add") (id 80)
			(e-lookup-local @4-5-4-6
				(pattern (id 72)))
			(e-int @4-9-4-10 (num-var 79) (value "1")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "a") (type "Num(*)"))
		(def (name "b") (type "*")))
	(expressions
		(expr @3-5-3-6 (type "Num(*)"))
		(expr @4-5-4-10 (type "*"))))
~~~