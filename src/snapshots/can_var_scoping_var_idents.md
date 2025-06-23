# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# Function showing var vs regular identifier independence
testFunc = |input| {
	sum = input # Regular identifier
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:58),
LowerIdent(4:1-4:9),OpAssign(4:10-4:11),OpBar(4:12-4:13),LowerIdent(4:13-4:18),OpBar(4:18-4:19),OpenCurly(4:20-4:21),Newline(1:1-1:1),
LowerIdent(5:2-5:5),OpAssign(5:6-5:7),LowerIdent(5:8-5:13),Newline(5:15-5:34),
KwVar(6:2-6:5),LowerIdent(6:6-6:10),OpAssign(6:11-6:12),LowerIdent(6:13-6:18),OpStar(6:19-6:20),Int(6:21-6:22),Newline(6:24-6:66),
Newline(1:1-1:1),
LowerIdent(8:2-8:6),OpAssign(8:7-8:8),LowerIdent(8:9-8:13),OpPlus(8:14-8:15),LowerIdent(8:16-8:19),Newline(8:21-8:48),
LowerIdent(9:2-9:5),OpPlus(9:6-9:7),LowerIdent(9:8-9:12),Newline(9:14-9:40),
CloseCurly(10:1-10:2),EndOfFile(10:2-10:2),
~~~
# PARSE
~~~clojure
(file (1:1-10:2)
	(module (1:1-1:10) (exposes (1:8-1:10)))
	(statements
		(decl (4:1-10:2)
			(ident (4:1-4:9) "testFunc")
			(lambda (4:12-10:2)
				(args (ident (4:13-4:18) "input"))
				(block (4:20-10:2)
					(statements
						(decl (5:2-5:13)
							(ident (5:2-5:5) "sum")
							(ident (5:8-5:13) "" "input"))
						(var (6:2-8:6)
							(name "sum_")
							(binop (6:13-8:6)
								"*"
								(ident (6:13-6:18) "" "input")
								(int (6:21-6:22) "2")))
						(decl (8:2-9:5)
							(ident (8:2-8:6) "sum_")
							(binop (8:9-9:5)
								"+"
								(ident (8:9-8:13) "" "sum_")
								(ident (8:16-8:19) "" "sum")))
						(binop (9:2-10:2)
							"+"
							(ident (9:2-9:5) "" "sum")
							(ident (9:8-9:12) "" "sum_"))))))))
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
			(p_assign (4:1-4:9)
				(pid 12)
				(ident "testFunc")))
		(def_expr
			(e_lambda (4:12-10:2)
				(args
					(p_assign (4:13-4:18)
						(pid 13)
						(ident "input")))
				(e_block (4:20-10:2)
					(s_let (5:2-5:13)
						(p_assign (5:2-5:5)
							(pid 14)
							(ident "sum"))
						(e_lookup (5:8-5:13) (pid 13)))
					(s_var (6:2-8:6)
						(pid 22)
						(p_assign (6:2-8:6)
							(pid 22)
							(ident "sum_"))
						(e_binop (6:13-8:6)
							"mul"
							(e_lookup (6:13-6:18) (pid 13))
							(e_int (6:21-6:22)
								(int_var 19)
								(precision_var 18)
								(literal "2")
								(value "TODO")
								(bound "u8"))))
					(s_reassign (8:2-8:6)
						(pid 22)
						(e_binop (8:9-9:5)
							"add"
							(e_lookup (8:9-8:13) (pid 22))
							(e_lookup (8:16-8:19) (pid 14))))
					(e_binop (9:2-10:2)
						"add"
						(e_lookup (9:2-9:5) (pid 14))
						(e_lookup (9:8-9:12) (pid 22))))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "testFunc" 33 (type "*")))
	(expressions
		(expr (4:12-10:2) 32 (type "*"))))
~~~