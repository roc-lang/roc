# META
~~~ini
description=Block variables with unused variable checking
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
    # Regular unused variable - should warn
    unused_var = 42

    # Regular used variable - should be fine
    used_var = 100

    # Another unused variable - should warn
    another_unused = "hello"

    # Underscore variable that is unused - should be fine
    _ignored = 999

    # Use only the used_var
    result = used_var + 10
    result
}
~~~
# PROBLEMS
**UNUSED VARIABLE**
Variable ``unused_var`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_unused_var` to suppress this warning.
The unused variable is declared here:
**unused_vars_block.md:5:5:5:15:**
```roc
    unused_var = 42
```


**UNUSED VARIABLE**
Variable ``another_unused`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_another_unused` to suppress this warning.
The unused variable is declared here:
**unused_vars_block.md:11:5:11:19:**
```roc
    another_unused = "hello"
```


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:6),OpAssign(3:7-3:8),OpBar(3:9-3:10),Underscore(3:10-3:11),OpBar(3:11-3:12),OpenCurly(3:13-3:14),Newline(1:1-1:1),
Newline(4:6-4:44),
LowerIdent(5:5-5:15),OpAssign(5:16-5:17),Int(5:18-5:20),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(7:6-7:45),
LowerIdent(8:5-8:13),OpAssign(8:14-8:15),Int(8:16-8:19),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(10:6-10:44),
LowerIdent(11:5-11:19),OpAssign(11:20-11:21),StringStart(11:22-11:23),StringPart(11:23-11:28),StringEnd(11:28-11:29),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(13:6-13:58),
NamedUnderscore(14:5-14:13),OpAssign(14:14-14:15),Int(14:16-14:19),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(16:6-16:28),
LowerIdent(17:5-17:11),OpAssign(17:12-17:13),LowerIdent(17:14-17:22),OpPlus(17:23-17:24),Int(17:25-17:27),Newline(1:1-1:1),
LowerIdent(18:5-18:11),Newline(1:1-1:1),
CloseCurly(19:1-19:2),EndOfFile(19:2-19:2),
~~~
# PARSE
~~~clojure
(file (1:1-19:2)
	(app (1:1-1:53)
		(provides (1:6-1:12) (exposed_item (lower_ident "main!")))
		(record_field (1:15-1:53)
			"pf"
			(string (1:28-1:51) (string_part (1:29-1:50) "../basic-cli/main.roc")))
		(packages (1:13-1:53)
			(record_field (1:15-1:53)
				"pf"
				(string (1:28-1:51) (string_part (1:29-1:50) "../basic-cli/main.roc")))))
	(statements
		(decl (3:1-19:2)
			(ident (3:1-3:6) "main!")
			(lambda (3:9-19:2)
				(args (underscore))
				(block (3:13-19:2)
					(statements
						(decl (5:5-5:20)
							(ident (5:5-5:15) "unused_var")
							(int (5:18-5:20) "42"))
						(decl (8:5-8:19)
							(ident (8:5-8:13) "used_var")
							(int (8:16-8:19) "100"))
						(decl (11:5-11:29)
							(ident (11:5-11:19) "another_unused")
							(string (11:22-11:29) (string_part (11:23-11:28) "hello")))
						(decl (14:5-14:19)
							(ident (14:5-14:13) "_ignored")
							(int (14:16-14:19) "999"))
						(decl (17:5-18:11)
							(ident (17:5-17:11) "result")
							(binop (17:14-18:11)
								"+"
								(ident (17:14-17:22) "" "used_var")
								(int (17:25-17:27) "10")))
						(ident (18:5-18:11) "" "result")))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

main! = |_| {
	# Regular unused variable - should warn
	unused_var = 42

	# Regular used variable - should be fine
	used_var = 100

	# Another unused variable - should warn
	another_unused = "hello"

	# Underscore variable that is unused - should be fine
	_ignored = 999

	# Use only the used_var
	result = used_var + 10
	result
}
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (3:1-3:6)
				(pid 72)
				(ident "main!")))
		(def_expr
			(e_lambda (3:9-19:2)
				(args (p_underscore (3:10-3:11) (pid 73)))
				(e_block (3:13-19:2)
					(s_let (5:5-5:20)
						(p_assign (5:5-5:15)
							(pid 74)
							(ident "unused_var"))
						(e_int (5:18-5:20)
							(int_var 76)
							(precision_var 75)
							(literal "42")
							(value "TODO")
							(bound "u8")))
					(s_let (8:5-8:19)
						(p_assign (8:5-8:13)
							(pid 79)
							(ident "used_var"))
						(e_int (8:16-8:19)
							(int_var 81)
							(precision_var 80)
							(literal "100")
							(value "TODO")
							(bound "u8")))
					(s_let (11:5-11:29)
						(p_assign (11:5-11:19)
							(pid 84)
							(ident "another_unused"))
						(e_string (11:22-11:29) (e_literal (11:23-11:28) "hello")))
					(s_let (14:5-14:19)
						(p_assign (14:5-14:13)
							(pid 88)
							(ident "_ignored"))
						(e_int (14:16-14:19)
							(int_var 90)
							(precision_var 89)
							(literal "999")
							(value "TODO")
							(bound "u8")))
					(s_let (17:5-18:11)
						(p_assign (17:5-17:11)
							(pid 93)
							(ident "result"))
						(e_binop (17:14-18:11)
							"add"
							(e_lookup (17:14-17:22) (pid 79))
							(e_int (17:25-17:27)
								(int_var 96)
								(precision_var 95)
								(literal "10")
								(value "TODO")
								(bound "u8"))))
					(e_lookup (18:5-18:11) (pid 93)))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "main!" 105 (type "*")))
	(expressions
		(expr (3:9-19:2) 104 (type "*"))))
~~~