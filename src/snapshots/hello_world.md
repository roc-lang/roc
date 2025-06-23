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
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: top-level import
**UNDEFINED VARIABLE**
Nothing is named ``line!`` in this scope.
Is there an `import` or `exposing` missing up-top?
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),LowerIdent(3:8-3:10),NoSpaceDotUpperIdent(3:10-3:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:6),OpAssign(5:7-5:8),OpBar(5:9-5:10),Underscore(5:10-5:11),OpBar(5:11-5:12),UpperIdent(5:13-5:19),NoSpaceDotLowerIdent(5:19-5:25),NoSpaceOpenRound(5:25-5:26),StringStart(5:26-5:27),StringPart(5:27-5:40),StringEnd(5:40-5:41),CloseRound(5:41-5:42),EndOfFile(5:42-5:42),
~~~
# PARSE
~~~clojure
(file (1:1-5:42)
	(app (1:1-1:57)
		(provides (1:6-1:12) (exposed_item (lower_ident "main!")))
		(record_field (1:15-1:57)
			"pf"
			(string (1:28-1:55) (string_part (1:29-1:54) "../basic-cli/platform.roc")))
		(packages (1:13-1:57)
			(record_field (1:15-1:57)
				"pf"
				(string (1:28-1:55) (string_part (1:29-1:54) "../basic-cli/platform.roc")))))
	(statements
		(import (3:1-3:17) ".Stdout" (qualifier "pf"))
		(decl (5:1-5:42)
			(ident (5:1-5:6) "main!")
			(lambda (5:9-5:42)
				(args (underscore))
				(apply (5:13-5:42)
					(ident (5:13-5:25) "Stdout" ".line!")
					(string (5:26-5:41) (string_part (5:27-5:40) "Hello, world!")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (5:1-5:6)
				(pid 13)
				(ident "main!")))
		(def_expr
			(e_lambda (5:9-5:42)
				(args (p_underscore (5:10-5:11) (pid 14)))
				(e_call (5:13-5:42)
					(e_runtime_error (5:13-5:25) "ident_not_in_scope")
					(e_string (5:26-5:41) (e_literal (5:27-5:40) "Hello, world!")))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "main!" 21 (type "*")))
	(expressions
		(expr (5:9-5:42) 20 (type "*"))))
~~~