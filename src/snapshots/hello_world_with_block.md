# META
~~~ini
description=Hello world with a block
type=file
~~~
# SOURCE
~~~roc
# Hello world!

# Multiline comments?
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

main! = |_| {
	world = "World"
	# Hello
	Stdout.line!("Hello, world!")
}
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: top-level import

**UNDEFINED VARIABLE**
Nothing is named `line!` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNUSED VARIABLE**
Variable ``world`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_world` to suppress this warning.
The unused variable is declared here:
**hello_world_with_block.md:9:2:9:7:**
```roc
	world = "World"
```


# TOKENS
~~~zig
Newline(1:2-1:15),
Newline(1:1-1:1),
Newline(3:2-3:22),
KwApp(4:1-4:4),OpenSquare(4:5-4:6),LowerIdent(4:6-4:11),CloseSquare(4:11-4:12),OpenCurly(4:13-4:14),LowerIdent(4:15-4:17),OpColon(4:17-4:18),KwPlatform(4:19-4:27),StringStart(4:28-4:29),StringPart(4:29-4:54),StringEnd(4:54-4:55),CloseCurly(4:56-4:57),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(6:1-6:7),LowerIdent(6:8-6:10),NoSpaceDotUpperIdent(6:10-6:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(8:1-8:6),OpAssign(8:7-8:8),OpBar(8:9-8:10),Underscore(8:10-8:11),OpBar(8:11-8:12),OpenCurly(8:13-8:14),Newline(1:1-1:1),
LowerIdent(9:2-9:7),OpAssign(9:8-9:9),StringStart(9:10-9:11),StringPart(9:11-9:16),StringEnd(9:16-9:17),Newline(1:1-1:1),
Newline(10:3-10:9),
UpperIdent(11:2-11:8),NoSpaceDotLowerIdent(11:8-11:14),NoSpaceOpenRound(11:14-11:15),StringStart(11:15-11:16),StringPart(11:16-11:29),StringEnd(11:29-11:30),CloseRound(11:30-11:31),Newline(1:1-1:1),
CloseCurly(12:1-12:2),EndOfFile(12:2-12:2),
~~~
# PARSE
~~~clojure
(file (1:2-12:2)
	(app (4:1-4:57)
		(provides (4:6-4:12) (exposed_item (lower_ident "main!")))
		(record_field (4:15-4:57)
			"pf"
			(string (4:28-4:55) (string_part (4:29-4:54) "../basic-cli/platform.roc")))
		(packages (4:13-4:57)
			(record_field (4:15-4:57)
				"pf"
				(string (4:28-4:55) (string_part (4:29-4:54) "../basic-cli/platform.roc")))))
	(statements
		(import (6:1-6:17) ".Stdout" (qualifier "pf"))
		(decl (8:1-12:2)
			(ident (8:1-8:6) "main!")
			(lambda (8:9-12:2)
				(args (underscore))
				(block (8:13-12:2)
					(statements
						(decl (9:2-9:17)
							(ident (9:2-9:7) "world")
							(string (9:10-9:17) (string_part (9:11-9:16) "World")))
						(apply (11:2-11:31)
							(ident (11:2-11:14) "Stdout" ".line!")
							(string (11:15-11:30) (string_part (11:16-11:29) "Hello, world!")))))))))
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
			(p_assign (8:1-8:6)
				(pid 73)
				(ident "main!")))
		(def_expr
			(e_lambda (8:9-12:2)
				(args (p_underscore (8:10-8:11) (pid 74)))
				(e_block (8:13-12:2)
					(s_let (9:2-9:17)
						(p_assign (9:2-9:7)
							(pid 75)
							(ident "world"))
						(e_string (9:10-9:17) (e_literal (9:11-9:16) "World")))
					(e_call (11:2-11:31)
						(e_runtime_error (11:2-11:14) "ident_not_in_scope")
						(e_string (11:15-11:30) (e_literal (11:16-11:29) "Hello, world!"))))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "main!" 87 (type "*")))
	(expressions
		(expr (8:9-12:2) 86 (type "*"))))
~~~