# META
~~~ini
description=Two decls
type=file
~~~
# SOURCE
~~~roc
app [] { pf: platform "../basic-cli/platform.roc" }

a = 5
b = a + 1
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),CloseSquare(1:6-1:7),OpenCurly(1:8-1:9),LowerIdent(1:10-1:12),OpColon(1:12-1:13),KwPlatform(1:14-1:22),StringStart(1:23-1:24),StringPart(1:24-1:49),StringEnd(1:49-1:50),CloseCurly(1:51-1:52),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Int(3:5-3:6),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),LowerIdent(4:5-4:6),OpPlus(4:7-4:8),Int(4:9-4:10),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.10
	(app @1.1-1.52
		(provides @1.5-1.7)
		(record-field @1.10-1.50 (name "pf")
			(e-string @1.23-1.50
				(e-string-part @1.24-1.49 (raw "../basic-cli/platform.roc"))))
		(packages @1.8-1.52
			(record-field @1.10-1.50 (name "pf")
				(e-string @1.23-1.50
					(e-string-part @1.24-1.49 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-decl @3.1-3.6
			(p-ident @3.1-3.2 (raw "a"))
			(e-int @3.5-3.6 (raw "5")))
		(s-decl @4.1-4.10
			(p-ident @4.1-4.2 (raw "b"))
			(e-binop @4.5-4.10 (op "+")
				(e-ident @4.5-4.6 (raw "a"))
				(e-int @4.9-4.10 (raw "1"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.2 (ident "a"))
		(e-num @3.5-3.6 (value "5")))
	(d-let
		(p-assign @4.1-4.2 (ident "b"))
		(e-binop @4.5-4.10 (op "add")
			(e-lookup-local @4.5-4.6
				(p-assign @3.1-3.2 (ident "a")))
			(e-num @4.9-4.10 (value "1")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "Num(_size)"))
		(patt @4.1-4.2 (type "Num(_size)")))
	(expressions
		(expr @3.5-3.6 (type "Num(_size)"))
		(expr @4.5-4.10 (type "Num(_size)"))))
~~~
