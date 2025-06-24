# META
~~~ini
description=Simple unused and used underscore variable test
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Regular variable that is unused - should warn
unused_regular = |x| 42

# Underscore variable that is used - should warn
used_underscore = |_value| _value

# Underscore variable that is unused - should be fine
unused_underscore = |_ignored| 100

# Regular variable that is used - should be fine
used_regular = |number| number + 1

main! = |_| {
    a = unused_regular(5)
    b = used_underscore(10)
    c = unused_underscore(15)
    d = used_regular(20)
    a + b + c + d
}
~~~
# PROBLEMS
**UNUSED VARIABLE**
Variable ``x`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**unused_vars_simple.md:4:19:4:20:**
```roc
unused_regular = |x| 42
```


**UNDERSCORE VARIABLE USED**
Variable ``_value`` is prefixed with an underscore but is actually used.

Variables prefixed with `_` are intended to be unused. Remove the underscore prefix: `value`.
The underscore variable is declared here:
**unused_vars_simple.md:7:28:7:34:**
```roc
used_underscore = |_value| _value
```


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:48),
LowerIdent(4:1-4:15),OpAssign(4:16-4:17),OpBar(4:18-4:19),LowerIdent(4:19-4:20),OpBar(4:20-4:21),Int(4:22-4:24),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:2-6:49),
LowerIdent(7:1-7:16),OpAssign(7:17-7:18),OpBar(7:19-7:20),NamedUnderscore(7:20-7:26),OpBar(7:26-7:27),NamedUnderscore(7:28-7:34),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(9:2-9:54),
LowerIdent(10:1-10:18),OpAssign(10:19-10:20),OpBar(10:21-10:22),NamedUnderscore(10:22-10:30),OpBar(10:30-10:31),Int(10:32-10:35),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(12:2-12:49),
LowerIdent(13:1-13:13),OpAssign(13:14-13:15),OpBar(13:16-13:17),LowerIdent(13:17-13:23),OpBar(13:23-13:24),LowerIdent(13:25-13:31),OpPlus(13:32-13:33),Int(13:34-13:35),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(15:1-15:6),OpAssign(15:7-15:8),OpBar(15:9-15:10),Underscore(15:10-15:11),OpBar(15:11-15:12),OpenCurly(15:13-15:14),Newline(1:1-1:1),
LowerIdent(16:5-16:6),OpAssign(16:7-16:8),LowerIdent(16:9-16:23),NoSpaceOpenRound(16:23-16:24),Int(16:24-16:25),CloseRound(16:25-16:26),Newline(1:1-1:1),
LowerIdent(17:5-17:6),OpAssign(17:7-17:8),LowerIdent(17:9-17:24),NoSpaceOpenRound(17:24-17:25),Int(17:25-17:27),CloseRound(17:27-17:28),Newline(1:1-1:1),
LowerIdent(18:5-18:6),OpAssign(18:7-18:8),LowerIdent(18:9-18:26),NoSpaceOpenRound(18:26-18:27),Int(18:27-18:29),CloseRound(18:29-18:30),Newline(1:1-1:1),
LowerIdent(19:5-19:6),OpAssign(19:7-19:8),LowerIdent(19:9-19:21),NoSpaceOpenRound(19:21-19:22),Int(19:22-19:24),CloseRound(19:24-19:25),Newline(1:1-1:1),
LowerIdent(20:5-20:6),OpPlus(20:7-20:8),LowerIdent(20:9-20:10),OpPlus(20:11-20:12),LowerIdent(20:13-20:14),OpPlus(20:15-20:16),LowerIdent(20:17-20:18),Newline(1:1-1:1),
CloseCurly(21:1-21:2),EndOfFile(21:2-21:2),
~~~
# PARSE
~~~clojure
(file (1:1-21:2)
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
		(decl (4:1-4:24)
			(ident (4:1-4:15) "unused_regular")
			(lambda (4:18-4:24)
				(args (ident (4:19-4:20) "x"))
				(int (4:22-4:24) "42")))
		(decl (7:1-7:34)
			(ident (7:1-7:16) "used_underscore")
			(lambda (7:19-7:34)
				(args (ident (7:20-7:26) "_value"))
				(ident (7:28-7:34) "" "_value")))
		(decl (10:1-10:35)
			(ident (10:1-10:18) "unused_underscore")
			(lambda (10:21-10:35)
				(args (ident (10:22-10:30) "_ignored"))
				(int (10:32-10:35) "100")))
		(decl (13:1-15:6)
			(ident (13:1-13:13) "used_regular")
			(lambda (13:16-15:6)
				(args (ident (13:17-13:23) "number"))
				(binop (13:25-15:6)
					"+"
					(ident (13:25-13:31) "" "number")
					(int (13:34-13:35) "1"))))
		(decl (15:1-21:2)
			(ident (15:1-15:6) "main!")
			(lambda (15:9-21:2)
				(args (underscore))
				(block (15:13-21:2)
					(statements
						(decl (16:5-16:26)
							(ident (16:5-16:6) "a")
							(apply (16:9-16:26)
								(ident (16:9-16:23) "" "unused_regular")
								(int (16:24-16:25) "5")))
						(decl (17:5-17:28)
							(ident (17:5-17:6) "b")
							(apply (17:9-17:28)
								(ident (17:9-17:24) "" "used_underscore")
								(int (17:25-17:27) "10")))
						(decl (18:5-18:30)
							(ident (18:5-18:6) "c")
							(apply (18:9-18:30)
								(ident (18:9-18:26) "" "unused_underscore")
								(int (18:27-18:29) "15")))
						(decl (19:5-19:25)
							(ident (19:5-19:6) "d")
							(apply (19:9-19:25)
								(ident (19:9-19:21) "" "used_regular")
								(int (19:22-19:24) "20")))
						(binop (20:5-21:2)
							"+"
							(ident (20:5-20:6) "" "a")
							(binop (20:9-21:2)
								"+"
								(ident (20:9-20:10) "" "b")
								(binop (20:13-21:2)
									"+"
									(ident (20:13-20:14) "" "c")
									(ident (20:17-20:18) "" "d"))))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Regular variable that is unused - should warn
unused_regular = |x| 42

# Underscore variable that is used - should warn
used_underscore = |_value| _value

# Underscore variable that is unused - should be fine
unused_underscore = |_ignored| 100

# Regular variable that is used - should be fine
used_regular = |number| number + 1

main! = |_| {
	a = unused_regular(5)
	b = used_underscore(10)
	c = unused_underscore(15)
	d = used_regular(20)
	a + b + c + d
}
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (4:1-4:15)
				(pid 72)
				(ident "unused_regular")))
		(def_expr
			(e_lambda (4:18-4:24)
				(args
					(p_assign (4:19-4:20)
						(pid 73)
						(ident "x")))
				(e_int (4:22-4:24)
					(int_var 75)
					(precision_var 74)
					(literal "42")
					(value "TODO")
					(bound "u8")))))
	(d_let
		(def_pattern
			(p_assign (7:1-7:16)
				(pid 80)
				(ident "used_underscore")))
		(def_expr
			(e_lambda (7:19-7:34)
				(args
					(p_assign (7:20-7:26)
						(pid 81)
						(ident "_value")))
				(e_lookup_local (7:28-7:34) (pid 81)))))
	(d_let
		(def_pattern
			(p_assign (10:1-10:18)
				(pid 86)
				(ident "unused_underscore")))
		(def_expr
			(e_lambda (10:21-10:35)
				(args
					(p_assign (10:22-10:30)
						(pid 87)
						(ident "_ignored")))
				(e_int (10:32-10:35)
					(int_var 89)
					(precision_var 88)
					(literal "100")
					(value "TODO")
					(bound "u8")))))
	(d_let
		(def_pattern
			(p_assign (13:1-13:13)
				(pid 93)
				(ident "used_regular")))
		(def_expr
			(e_lambda (13:16-15:6)
				(args
					(p_assign (13:17-13:23)
						(pid 94)
						(ident "number")))
				(e_binop (13:25-15:6)
					"add"
					(e_lookup_local (13:25-13:31) (pid 94))
					(e_int (13:34-13:35)
						(int_var 97)
						(precision_var 96)
						(literal "1")
						(value "TODO")
						(bound "u8"))))))
	(d_let
		(def_pattern
			(p_assign (15:1-15:6)
				(pid 102)
				(ident "main!")))
		(def_expr
			(e_lambda (15:9-21:2)
				(args (p_underscore (15:10-15:11) (pid 103)))
				(e_block (15:13-21:2)
					(s_let (16:5-16:26)
						(p_assign (16:5-16:6)
							(pid 104)
							(ident "a"))
						(e_call (16:9-16:26)
							(e_lookup_local (16:9-16:23) (pid 72))
							(e_int (16:24-16:25)
								(int_var 107)
								(precision_var 106)
								(literal "5")
								(value "TODO")
								(bound "u8"))))
					(s_let (17:5-17:28)
						(p_assign (17:5-17:6)
							(pid 111)
							(ident "b"))
						(e_call (17:9-17:28)
							(e_lookup_local (17:9-17:24) (pid 80))
							(e_int (17:25-17:27)
								(int_var 114)
								(precision_var 113)
								(literal "10")
								(value "TODO")
								(bound "u8"))))
					(s_let (18:5-18:30)
						(p_assign (18:5-18:6)
							(pid 118)
							(ident "c"))
						(e_call (18:9-18:30)
							(e_lookup_local (18:9-18:26) (pid 86))
							(e_int (18:27-18:29)
								(int_var 121)
								(precision_var 120)
								(literal "15")
								(value "TODO")
								(bound "u8"))))
					(s_let (19:5-19:25)
						(p_assign (19:5-19:6)
							(pid 125)
							(ident "d"))
						(e_call (19:9-19:25)
							(e_lookup_local (19:9-19:21) (pid 93))
							(e_int (19:22-19:24)
								(int_var 128)
								(precision_var 127)
								(literal "20")
								(value "TODO")
								(bound "u8"))))
					(e_binop (20:5-21:2)
						"add"
						(e_lookup_local (20:5-20:6) (pid 104))
						(e_binop (20:9-21:2)
							"add"
							(e_lookup_local (20:9-20:10) (pid 111))
							(e_binop (20:13-21:2)
								"add"
								(e_lookup_local (20:13-20:14) (pid 118))
								(e_lookup_local (20:17-20:18) (pid 125))))))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "unused_regular" 79 (type "*"))
		(def "used_underscore" 85 (type "*"))
		(def "unused_underscore" 92 (type "*"))
		(def "used_regular" 101 (type "*"))
		(def "main!" 141 (type "*")))
	(expressions
		(expr (4:18-4:24) 77 (type "*"))
		(expr (7:19-7:34) 84 (type "*"))
		(expr (10:21-10:35) 91 (type "*"))
		(expr (13:16-15:6) 100 (type "*"))
		(expr (15:9-21:2) 140 (type "*"))))
~~~