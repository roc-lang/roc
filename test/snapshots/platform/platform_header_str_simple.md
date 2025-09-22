# META
~~~ini
description=Simple plaform module
type=file
~~~
# SOURCE
~~~roc
platform ""
	requires {} { main : Str -> Str }
	exposes []
	packages {}
	provides { entrypoint: "roc__entrypoint" }

entrypoint : Str -> Str
entrypoint = main
~~~
# EXPECTED
UNDEFINED VARIABLE - platform_header_str_simple.md:8:14:8:18
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `main` in this scope.
Is there an `import` or `exposing` missing up-top?

**platform_header_str_simple.md:8:14:8:18:**
```roc
entrypoint = main
```
             ^^^^


# TOKENS
~~~zig
KwPlatform(1:1-1:9),StringStart(1:10-1:11),StringPart(1:11-1:11),StringEnd(1:11-1:12),
KwRequires(2:2-2:10),OpenCurly(2:11-2:12),CloseCurly(2:12-2:13),OpenCurly(2:14-2:15),LowerIdent(2:16-2:20),OpColon(2:21-2:22),UpperIdent(2:23-2:26),OpArrow(2:27-2:29),UpperIdent(2:30-2:33),CloseCurly(2:34-2:35),
KwExposes(3:2-3:9),OpenSquare(3:10-3:11),CloseSquare(3:11-3:12),
KwPackages(4:2-4:10),OpenCurly(4:11-4:12),CloseCurly(4:12-4:13),
KwProvides(5:2-5:10),OpenCurly(5:11-5:12),LowerIdent(5:13-5:23),OpColon(5:23-5:24),StringStart(5:25-5:26),StringPart(5:26-5:41),StringEnd(5:41-5:42),CloseCurly(5:43-5:44),
LowerIdent(7:1-7:11),OpColon(7:12-7:13),UpperIdent(7:14-7:17),OpArrow(7:18-7:20),UpperIdent(7:21-7:24),
LowerIdent(8:1-8:11),OpAssign(8:12-8:13),LowerIdent(8:14-8:18),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @1.1-8.18
	(platform @1.1-5.44 (name "")
		(rigids @2.11-2.13)
		(ty-record @2.14-2.35
			(anno-record-field @2.16-2.33 (name "main")
				(ty-fn @2.23-2.33
					(ty @2.23-2.26 (name "Str"))
					(ty @2.30-2.33 (name "Str")))))
		(exposes @3.10-3.12)
		(packages @4.11-4.13)
		(provides @5.11-5.44
			(record-field @5.13-5.42 (name "entrypoint")
				(e-string @5.25-5.42
					(e-string-part @5.26-5.41 (raw "roc__entrypoint"))))))
	(statements
		(s-type-anno @7.1-7.24 (name "entrypoint")
			(ty-fn @7.14-7.24
				(ty @7.14-7.17 (name "Str"))
				(ty @7.21-7.24 (name "Str"))))
		(s-decl @8.1-8.18
			(p-ident @8.1-8.11 (raw "entrypoint"))
			(e-ident @8.14-8.18 (raw "main")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @8.1-8.11 (ident "entrypoint"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation @8.1-8.11
			(declared-type
				(ty-fn @7.14-7.24 (effectful false)
					(ty-lookup @7.14-7.17 (name "Str") (builtin))
					(ty-lookup @7.21-7.24 (name "Str") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.11 (type "Error")))
	(expressions
		(expr @8.14-8.18 (type "Error"))))
~~~
