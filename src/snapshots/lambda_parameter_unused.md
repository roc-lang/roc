# META
~~~ini
description=Lambda parameters with unused variable checking
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Lambda with unused parameter - should warn
add : U64 -> U64
add = |unused| 42

# Lambda with underscore parameter that is used - should warn
multiply : U64 -> U64
multiply = |_factor| _factor * 2

# Lambda with unused underscore parameter - should be fine
process : U64 -> U64
process = |_input| 100

# Lambda with used parameter - should be fine
double : U64 -> U64
double = |value| value * 2

main! = |_| {
    result1 = add(5)
    result2 = multiply(3)
    result3 = process(7)
    result4 = double(4)
    result1 + result2 + result3 + result4
}
~~~
# PROBLEMS
**UNUSED VARIABLE**
Variable ``unused`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_unused` to suppress this warning.
The unused variable is declared here:
**lambda_parameter_unused.md:6:9:6:15:**
```roc

```


**UNDERSCORE VARIABLE USED**
Variable ``_factor`` is prefixed with an underscore but is actually used.

Variables prefixed with `_` are intended to be unused. Remove the underscore prefix: `factor`.
The underscore variable is declared here:
**lambda_parameter_unused.md:10:23:10:30:**
```roc

```


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:45),
LowerIdent(4:1-4:4),OpColon(4:5-4:6),UpperIdent(4:7-4:10),OpArrow(4:11-4:13),UpperIdent(4:14-4:17),Newline(1:1-1:1),
LowerIdent(5:1-5:4),OpAssign(5:5-5:6),OpBar(5:7-5:8),LowerIdent(5:8-5:14),OpBar(5:14-5:15),Int(5:16-5:18),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(7:2-7:62),
LowerIdent(8:1-8:9),OpColon(8:10-8:11),UpperIdent(8:12-8:15),OpArrow(8:16-8:18),UpperIdent(8:19-8:22),Newline(1:1-1:1),
LowerIdent(9:1-9:9),OpAssign(9:10-9:11),OpBar(9:12-9:13),NamedUnderscore(9:13-9:20),OpBar(9:20-9:21),NamedUnderscore(9:22-9:29),OpStar(9:30-9:31),Int(9:32-9:33),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(11:2-11:59),
LowerIdent(12:1-12:8),OpColon(12:9-12:10),UpperIdent(12:11-12:14),OpArrow(12:15-12:17),UpperIdent(12:18-12:21),Newline(1:1-1:1),
LowerIdent(13:1-13:8),OpAssign(13:9-13:10),OpBar(13:11-13:12),NamedUnderscore(13:12-13:18),OpBar(13:18-13:19),Int(13:20-13:23),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(15:2-15:46),
LowerIdent(16:1-16:7),OpColon(16:8-16:9),UpperIdent(16:10-16:13),OpArrow(16:14-16:16),UpperIdent(16:17-16:20),Newline(1:1-1:1),
LowerIdent(17:1-17:7),OpAssign(17:8-17:9),OpBar(17:10-17:11),LowerIdent(17:11-17:16),OpBar(17:16-17:17),LowerIdent(17:18-17:23),OpStar(17:24-17:25),Int(17:26-17:27),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(19:1-19:6),OpAssign(19:7-19:8),OpBar(19:9-19:10),Underscore(19:10-19:11),OpBar(19:11-19:12),OpenCurly(19:13-19:14),Newline(1:1-1:1),
LowerIdent(20:5-20:12),OpAssign(20:13-20:14),LowerIdent(20:15-20:18),NoSpaceOpenRound(20:18-20:19),Int(20:19-20:20),CloseRound(20:20-20:21),Newline(1:1-1:1),
LowerIdent(21:5-21:12),OpAssign(21:13-21:14),LowerIdent(21:15-21:23),NoSpaceOpenRound(21:23-21:24),Int(21:24-21:25),CloseRound(21:25-21:26),Newline(1:1-1:1),
LowerIdent(22:5-22:12),OpAssign(22:13-22:14),LowerIdent(22:15-22:22),NoSpaceOpenRound(22:22-22:23),Int(22:23-22:24),CloseRound(22:24-22:25),Newline(1:1-1:1),
LowerIdent(23:5-23:12),OpAssign(23:13-23:14),LowerIdent(23:15-23:21),NoSpaceOpenRound(23:21-23:22),Int(23:22-23:23),CloseRound(23:23-23:24),Newline(1:1-1:1),
LowerIdent(24:5-24:12),OpPlus(24:13-24:14),LowerIdent(24:15-24:22),OpPlus(24:23-24:24),LowerIdent(24:25-24:32),OpPlus(24:33-24:34),LowerIdent(24:35-24:42),Newline(1:1-1:1),
CloseCurly(25:1-25:2),EndOfFile(25:2-25:2),
~~~
# PARSE
~~~clojure
(file (1:1-25:2)
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
		(type_anno (4:1-5:4)
			"add"
			(fn (4:7-4:17)
				(ty "U64")
				(ty "U64")))
		(decl (5:1-5:18)
			(ident (5:1-5:4) "add")
			(lambda (5:7-5:18)
				(args (ident (5:8-5:14) "unused"))
				(int (5:16-5:18) "42")))
		(type_anno (8:1-9:9)
			"multiply"
			(fn (8:12-8:22)
				(ty "U64")
				(ty "U64")))
		(decl (9:1-12:8)
			(ident (9:1-9:9) "multiply")
			(lambda (9:12-12:8)
				(args (ident (9:13-9:20) "_factor"))
				(binop (9:22-12:8)
					"*"
					(ident (9:22-9:29) "" "_factor")
					(int (9:32-9:33) "2"))))
		(type_anno (12:1-13:8)
			"process"
			(fn (12:11-12:21)
				(ty "U64")
				(ty "U64")))
		(decl (13:1-13:23)
			(ident (13:1-13:8) "process")
			(lambda (13:11-13:23)
				(args (ident (13:12-13:18) "_input"))
				(int (13:20-13:23) "100")))
		(type_anno (16:1-17:7)
			"double"
			(fn (16:10-16:20)
				(ty "U64")
				(ty "U64")))
		(decl (17:1-19:6)
			(ident (17:1-17:7) "double")
			(lambda (17:10-19:6)
				(args (ident (17:11-17:16) "value"))
				(binop (17:18-19:6)
					"*"
					(ident (17:18-17:23) "" "value")
					(int (17:26-17:27) "2"))))
		(decl (19:1-25:2)
			(ident (19:1-19:6) "main!")
			(lambda (19:9-25:2)
				(args (underscore))
				(block (19:13-25:2)
					(statements
						(decl (20:5-20:21)
							(ident (20:5-20:12) "result1")
							(apply (20:15-20:21)
								(ident (20:15-20:18) "" "add")
								(int (20:19-20:20) "5")))
						(decl (21:5-21:26)
							(ident (21:5-21:12) "result2")
							(apply (21:15-21:26)
								(ident (21:15-21:23) "" "multiply")
								(int (21:24-21:25) "3")))
						(decl (22:5-22:25)
							(ident (22:5-22:12) "result3")
							(apply (22:15-22:25)
								(ident (22:15-22:22) "" "process")
								(int (22:23-22:24) "7")))
						(decl (23:5-23:24)
							(ident (23:5-23:12) "result4")
							(apply (23:15-23:24)
								(ident (23:15-23:21) "" "double")
								(int (23:22-23:23) "4")))
						(binop (24:5-25:2)
							"+"
							(ident (24:5-24:12) "" "result1")
							(binop (24:15-25:2)
								"+"
								(ident (24:15-24:22) "" "result2")
								(binop (24:25-25:2)
									"+"
									(ident (24:25-24:32) "" "result3")
									(ident (24:35-24:42) "" "result4"))))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Lambda with unused parameter - should warn
add : U64 -> U64
add = |unused| 42

# Lambda with underscore parameter that is used - should warn
multiply : U64 -> U64
multiply = |_factor| _factor * 2

# Lambda with unused underscore parameter - should be fine
process : U64 -> U64
process = |_input| 100

# Lambda with used parameter - should be fine
double : U64 -> U64
double = |value| value * 2

main! = |_| {
	result1 = add(5)
	result2 = multiply(3)
	result3 = process(7)
	result4 = double(4)
	result1 + result2 + result3 + result4
}
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (5:1-5:4)
				(pid 75)
				(ident "add")))
		(def_expr
			(e_lambda (5:7-5:18)
				(args
					(p_assign (5:8-5:14)
						(pid 76)
						(ident "unused")))
				(e_int (5:16-5:18)
					(int_var 78)
					(precision_var 77)
					(literal "42")
					(value "TODO")
					(bound "u8"))))
		(annotation (5:1-5:4)
			(signature 85)
			(declared_type
				(fn (4:7-4:17)
					(ty (4:7-4:10) "U64")
					(ty (4:14-4:17) "U64")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (9:1-9:9)
				(pid 91)
				(ident "multiply")))
		(def_expr
			(e_lambda (9:12-12:8)
				(args
					(p_assign (9:13-9:20)
						(pid 92)
						(ident "_factor")))
				(e_binop (9:22-12:8)
					"mul"
					(e_lookup_local (9:22-9:29) (pid 92))
					(e_int (9:32-9:33)
						(int_var 96)
						(precision_var 95)
						(literal "2")
						(value "TODO")
						(bound "u8")))))
		(annotation (9:1-9:9)
			(signature 103)
			(declared_type
				(fn (8:12-8:22)
					(ty (8:12-8:15) "U64")
					(ty (8:19-8:22) "U64")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (13:1-13:8)
				(pid 109)
				(ident "process")))
		(def_expr
			(e_lambda (13:11-13:23)
				(args
					(p_assign (13:12-13:18)
						(pid 110)
						(ident "_input")))
				(e_int (13:20-13:23)
					(int_var 112)
					(precision_var 111)
					(literal "100")
					(value "TODO")
					(bound "u8"))))
		(annotation (13:1-13:8)
			(signature 118)
			(declared_type
				(fn (12:11-12:21)
					(ty (12:11-12:14) "U64")
					(ty (12:18-12:21) "U64")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (17:1-17:7)
				(pid 124)
				(ident "double")))
		(def_expr
			(e_lambda (17:10-19:6)
				(args
					(p_assign (17:11-17:16)
						(pid 125)
						(ident "value")))
				(e_binop (17:18-19:6)
					"mul"
					(e_lookup_local (17:18-17:23) (pid 125))
					(e_int (17:26-17:27)
						(int_var 128)
						(precision_var 127)
						(literal "2")
						(value "TODO")
						(bound "u8")))))
		(annotation (17:1-17:7)
			(signature 135)
			(declared_type
				(fn (16:10-16:20)
					(ty (16:10-16:13) "U64")
					(ty (16:17-16:20) "U64")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (19:1-19:6)
				(pid 138)
				(ident "main!")))
		(def_expr
			(e_lambda (19:9-25:2)
				(args (p_underscore (19:10-19:11) (pid 139)))
				(e_block (19:13-25:2)
					(s_let (20:5-20:21)
						(p_assign (20:5-20:12)
							(pid 140)
							(ident "result1"))
						(e_call (20:15-20:21)
							(e_lookup_local (20:15-20:18) (pid 75))
							(e_int (20:19-20:20)
								(int_var 143)
								(precision_var 142)
								(literal "5")
								(value "TODO")
								(bound "u8"))))
					(s_let (21:5-21:26)
						(p_assign (21:5-21:12)
							(pid 147)
							(ident "result2"))
						(e_call (21:15-21:26)
							(e_lookup_local (21:15-21:23) (pid 91))
							(e_int (21:24-21:25)
								(int_var 150)
								(precision_var 149)
								(literal "3")
								(value "TODO")
								(bound "u8"))))
					(s_let (22:5-22:25)
						(p_assign (22:5-22:12)
							(pid 154)
							(ident "result3"))
						(e_call (22:15-22:25)
							(e_lookup_local (22:15-22:22) (pid 109))
							(e_int (22:23-22:24)
								(int_var 157)
								(precision_var 156)
								(literal "7")
								(value "TODO")
								(bound "u8"))))
					(s_let (23:5-23:24)
						(p_assign (23:5-23:12)
							(pid 161)
							(ident "result4"))
						(e_call (23:15-23:24)
							(e_lookup_local (23:15-23:21) (pid 124))
							(e_int (23:22-23:23)
								(int_var 164)
								(precision_var 163)
								(literal "4")
								(value "TODO")
								(bound "u8"))))
					(e_binop (24:5-25:2)
						"add"
						(e_lookup_local (24:5-24:12) (pid 140))
						(e_binop (24:15-25:2)
							"add"
							(e_lookup_local (24:15-24:22) (pid 147))
							(e_binop (24:25-25:2)
								"add"
								(e_lookup_local (24:25-24:32) (pid 154))
								(e_lookup_local (24:35-24:42) (pid 161))))))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "add" 87 (type "*"))
		(def "multiply" 105 (type "*"))
		(def "process" 120 (type "*"))
		(def "double" 137 (type "*"))
		(def "main!" 177 (type "*")))
	(expressions
		(expr (5:7-5:18) 80 (type "*"))
		(expr (9:12-12:8) 99 (type "*"))
		(expr (13:11-13:23) 114 (type "*"))
		(expr (17:10-19:6) 131 (type "*"))
		(expr (19:9-25:2) 176 (type "*"))))
~~~