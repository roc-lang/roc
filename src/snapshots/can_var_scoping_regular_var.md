# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# Regular function with var usage
processItems = |items| {
	var count_ = 0
	var total_ = 0

	# Reassign vars within same function - should work
	count_ = count_ + 1
	total_ = total_ + 10

	# Nested function - var reassignment should fail across function boundary
	nestedFunc = |_| {
		count_ = count_ + 5 # Should cause error - different function
		total_ = total_ * 2 # Should cause error - different function
		count_
	}

	result = nestedFunc({})
	total_ + result
}
~~~
# PROBLEMS
**VAR REASSIGNMENT ERROR**
Cannot reassign a `var` from outside the function where it was declared.
Variables declared with `var` can only be reassigned within the same function scope.

**VAR REASSIGNMENT ERROR**
Cannot reassign a `var` from outside the function where it was declared.
Variables declared with `var` can only be reassigned within the same function scope.

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

**UNUSED VARIABLE**
Variable ``items`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_items` to suppress this warning.
The unused variable is declared here:
**can_var_scoping_regular_var.md:4:17:4:22:**
```roc
processItems = |items| {
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:34),
LowerIdent(4:1-4:13),OpAssign(4:14-4:15),OpBar(4:16-4:17),LowerIdent(4:17-4:22),OpBar(4:22-4:23),OpenCurly(4:24-4:25),Newline(1:1-1:1),
KwVar(5:2-5:5),LowerIdent(5:6-5:12),OpAssign(5:13-5:14),Int(5:15-5:16),Newline(1:1-1:1),
KwVar(6:2-6:5),LowerIdent(6:6-6:12),OpAssign(6:13-6:14),Int(6:15-6:16),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(8:3-8:52),
LowerIdent(9:2-9:8),OpAssign(9:9-9:10),LowerIdent(9:11-9:17),OpPlus(9:18-9:19),Int(9:20-9:21),Newline(1:1-1:1),
LowerIdent(10:2-10:8),OpAssign(10:9-10:10),LowerIdent(10:11-10:17),OpPlus(10:18-10:19),Int(10:20-10:22),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(12:3-12:75),
LowerIdent(13:2-13:12),OpAssign(13:13-13:14),OpBar(13:15-13:16),Underscore(13:16-13:17),OpBar(13:17-13:18),OpenCurly(13:19-13:20),Newline(1:1-1:1),
LowerIdent(14:3-14:9),OpAssign(14:10-14:11),LowerIdent(14:12-14:18),OpPlus(14:19-14:20),Int(14:21-14:22),Newline(14:24-14:64),
LowerIdent(15:3-15:9),OpAssign(15:10-15:11),LowerIdent(15:12-15:18),OpStar(15:19-15:20),Int(15:21-15:22),Newline(15:24-15:64),
LowerIdent(16:3-16:9),Newline(1:1-1:1),
CloseCurly(17:2-17:3),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(19:2-19:8),OpAssign(19:9-19:10),LowerIdent(19:11-19:21),NoSpaceOpenRound(19:21-19:22),OpenCurly(19:22-19:23),CloseCurly(19:23-19:24),CloseRound(19:24-19:25),Newline(1:1-1:1),
LowerIdent(20:2-20:8),OpPlus(20:9-20:10),LowerIdent(20:11-20:17),Newline(1:1-1:1),
CloseCurly(21:1-21:2),EndOfFile(21:2-21:2),
~~~
# PARSE
~~~clojure
(file (1:1-21:2)
	(module (1:1-1:10) (exposes (1:8-1:10)))
	(statements
		(decl (4:1-21:2)
			(ident (4:1-4:13) "processItems")
			(lambda (4:16-21:2)
				(args (ident (4:17-4:22) "items"))
				(block (4:24-21:2)
					(statements
						(var (5:2-6:5)
							(name "count_")
							(int (5:15-5:16) "0"))
						(var (6:2-9:8)
							(name "total_")
							(int (6:15-6:16) "0"))
						(decl (9:2-10:8)
							(ident (9:2-9:8) "count_")
							(binop (9:11-10:8)
								"+"
								(ident (9:11-9:17) "" "count_")
								(int (9:20-9:21) "1")))
						(decl (10:2-13:12)
							(ident (10:2-10:8) "total_")
							(binop (10:11-13:12)
								"+"
								(ident (10:11-10:17) "" "total_")
								(int (10:20-10:22) "10")))
						(decl (13:2-17:3)
							(ident (13:2-13:12) "nestedFunc")
							(lambda (13:15-17:3)
								(args (underscore))
								(block (13:19-17:3)
									(statements
										(decl (14:3-15:9)
											(ident (14:3-14:9) "count_")
											(binop (14:12-15:9)
												"+"
												(ident (14:12-14:18) "" "count_")
												(int (14:21-14:22) "5")))
										(decl (15:3-16:9)
											(ident (15:3-15:9) "total_")
											(binop (15:12-16:9)
												"*"
												(ident (15:12-15:18) "" "total_")
												(int (15:21-15:22) "2")))
										(ident (16:3-16:9) "" "count_")))))
						(decl (19:2-19:25)
							(ident (19:2-19:8) "result")
							(apply (19:11-19:25)
								(ident (19:11-19:21) "" "nestedFunc")
								(record (19:22-19:24))))
						(binop (20:2-21:2)
							"+"
							(ident (20:2-20:8) "" "total_")
							(ident (20:11-20:17) "" "result"))))))))
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
			(p_assign (4:1-4:13)
				(pid 72)
				(ident "processItems")))
		(def_expr
			(e_lambda (4:16-21:2)
				(args
					(p_assign (4:17-4:22)
						(pid 73)
						(ident "items")))
				(e_block (4:24-21:2)
					(s_var (5:2-6:5)
						(pid 77)
						(p_assign (5:2-6:5)
							(pid 77)
							(ident "count_"))
						(e_int (5:15-5:16)
							(int_var 75)
							(precision_var 74)
							(literal "0")
							(value "TODO")
							(bound "u8")))
					(s_var (6:2-9:8)
						(pid 82)
						(p_assign (6:2-9:8)
							(pid 82)
							(ident "total_"))
						(e_int (6:15-6:16)
							(int_var 80)
							(precision_var 79)
							(literal "0")
							(value "TODO")
							(bound "u8")))
					(s_reassign (9:2-9:8)
						(pid 77)
						(e_binop (9:11-10:8)
							"add"
							(e_lookup (9:11-9:17) (pid 77))
							(e_int (9:20-9:21)
								(int_var 86)
								(precision_var 85)
								(literal "1")
								(value "TODO")
								(bound "u8"))))
					(s_reassign (10:2-10:8)
						(pid 82)
						(e_binop (10:11-13:12)
							"add"
							(e_lookup (10:11-10:17) (pid 82))
							(e_int (10:20-10:22)
								(int_var 92)
								(precision_var 91)
								(literal "10")
								(value "TODO")
								(bound "u8"))))
					(s_let (13:2-17:3)
						(p_assign (13:2-13:12)
							(pid 96)
							(ident "nestedFunc"))
						(e_lambda (13:15-17:3)
							(args (p_underscore (13:16-13:17) (pid 97)))
							(e_block (13:19-17:3)
								(s_reassign (14:3-14:9)
									(pid 77)
									(e_runtime_error (14:3-14:9) "var_across_function_boundary"))
								(s_reassign (15:3-15:9)
									(pid 82)
									(e_runtime_error (15:3-15:9) "var_across_function_boundary"))
								(e_lookup (16:3-16:9) (pid 77)))))
					(s_let (19:2-19:25)
						(p_assign (19:2-19:8)
							(pid 108)
							(ident "result"))
						(e_call (19:11-19:25)
							(e_lookup (19:11-19:21) (pid 96))
							(e_runtime_error (1:1-1:1) "not_implemented")))
					(e_binop (20:2-21:2)
						"add"
						(e_lookup (20:2-20:8) (pid 82))
						(e_lookup (20:11-20:17) (pid 108))))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "processItems" 120 (type "*")))
	(expressions
		(expr (4:16-21:2) 118 (type "*"))))
~~~