# META
~~~ini
description=External declaration lookup from json module
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout
import json.Json

main! = |_| {
    # This should create an external declaration for json.Json.utf8
    result = Json.utf8("Hello from external module!")
    Stdout.line!(result)
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),LowerIdent(3:8-3:10),NoSpaceDotUpperIdent(3:10-3:17),Newline(1:1-1:1),
KwImport(4:1-4:7),LowerIdent(4:8-4:12),NoSpaceDotUpperIdent(4:12-4:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),OpenCurly(6:13-6:14),Newline(1:1-1:1),
Newline(7:6-7:68),
LowerIdent(8:5-8:11),OpAssign(8:12-8:13),UpperIdent(8:14-8:18),NoSpaceDotLowerIdent(8:18-8:23),NoSpaceOpenRound(8:23-8:24),StringStart(8:24-8:25),StringPart(8:25-8:52),StringEnd(8:52-8:53),CloseRound(8:53-8:54),Newline(1:1-1:1),
UpperIdent(9:5-9:11),NoSpaceDotLowerIdent(9:11-9:17),NoSpaceOpenRound(9:17-9:18),LowerIdent(9:18-9:24),CloseRound(9:24-9:25),Newline(1:1-1:1),
CloseCurly(10:1-10:2),EndOfFile(10:2-10:2),
~~~
# PARSE
~~~clojure
(file (1:1-10:2)
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
		(import (4:1-4:17) ".Json" (qualifier "json"))
		(decl (6:1-10:2)
			(ident (6:1-6:6) "main!")
			(lambda (6:9-10:2)
				(args (underscore))
				(block (6:13-10:2)
					(statements
						(decl (8:5-8:54)
							(ident (8:5-8:11) "result")
							(apply (8:14-8:54)
								(ident (8:14-8:23) "Json" ".utf8")
								(string (8:24-8:53) (string_part (8:25-8:52) "Hello from external module!"))))
						(apply (9:5-9:25)
							(ident (9:5-9:17) "Stdout" ".line!")
							(ident (9:18-9:24) "" "result"))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout
import json.Json

main! = |_| {
	# This should create an external declaration for json.Json.utf8
	result = Json.utf8("Hello from external module!")
	Stdout.line!(result)
}
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (6:1-6:6)
				(pid 74)
				(ident "main!")))
		(def_expr
			(e_lambda (6:9-10:2)
				(args (p_underscore (6:10-6:11) (pid 75)))
				(e_block (6:13-10:2)
					(s_let (8:5-8:54)
						(p_assign (8:5-8:11)
							(pid 76)
							(ident "result"))
						(e_call (8:14-8:54)
							(e_lookup_external
								(external_decl (8:14-8:23)
									(qualified_name "Json.utf8")
									(module_name "Json")
									(local_name "utf8")
									(kind "value")
									(type_var 77)))
							(e_string (8:24-8:53) (e_literal (8:25-8:52) "Hello from external module!"))))
					(e_call (9:5-9:25)
						(e_lookup_external
							(external_decl (9:5-9:17)
								(qualified_name "Stdout.line!")
								(module_name "Stdout")
								(local_name "line!")
								(kind "value")
								(type_var 83)))
						(e_lookup_local (9:18-9:24) (pid 76)))))))
	(s_import (3:1-3:17)
		"Stdout"
		""
		""
		(exposes))
	(s_import (4:1-4:17)
		"Json"
		""
		""
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "main!" 89 (type "*")))
	(expressions
		(expr (6:9-10:2) 88 (type "*"))))
~~~