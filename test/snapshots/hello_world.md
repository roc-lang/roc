# META
~~~ini
description=Hello world
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

main! = |_| Stdout.line!("Hello, world!")
~~~
# EXPECTED
MODULE NOT FOUND - hello_world.md:3:1:3:17
# PROBLEMS
**MODULE NOT FOUND**
The module `pf.Stdout` was not found in this Roc project.

You're attempting to use this module here:
**hello_world.md:3:1:3:17:**
```roc
import pf.Stdout
```
^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),
KwImport(3:1-3:7),LowerIdent(3:8-3:10),NoSpaceDotUpperIdent(3:10-3:17),
LowerIdent(5:1-5:6),OpAssign(5:7-5:8),OpBar(5:9-5:10),Underscore(5:10-5:11),OpBar(5:11-5:12),UpperIdent(5:13-5:19),NoSpaceDotLowerIdent(5:19-5:25),NoSpaceOpenRound(5:25-5:26),StringStart(5:26-5:27),StringPart(5:27-5:40),StringEnd(5:40-5:41),CloseRound(5:41-5:42),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.42
	(app @1.1-1.57
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.55 (name "pf")
			(e-string @1.28-1.55
				(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))
		(packages @1.13-1.57
			(record-field @1.15-1.55 (name "pf")
				(e-string @1.28-1.55
					(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-import @3.1-3.17 (raw "pf.Stdout"))
		(s-decl @5.1-5.42
			(p-ident @5.1-5.6 (raw "main!"))
			(e-lambda @5.9-5.42
				(args
					(p-underscore))
				(e-apply @5.13-5.42
					(e-ident @5.13-5.25 (raw "Stdout.line!"))
					(e-string @5.26-5.41
						(e-string-part @5.27-5.40 (raw "Hello, world!"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.6 (ident "main!"))
		(e-lambda @5.9-5.42
			(args
				(p-underscore @5.10-5.11))
			(e-call @5.13-5.42
				(e-lookup-external @5.13-5.25
					(module-idx "0")
					(target-node-idx "0"))
				(e-string @5.26-5.41
					(e-literal @5.27-5.40 (string "Hello, world!"))))))
	(s-import @3.1-3.17 (module "pf.Stdout") (qualifier "pf")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.6 (type "_arg -> _ret")))
	(expressions
		(expr @5.9-5.42 (type "_arg -> _ret"))))
~~~
