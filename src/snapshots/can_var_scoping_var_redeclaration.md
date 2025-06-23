# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# Test var redeclaration (should produce shadowing warning)
redeclareTest = |_| {
	var x_ = 5
	var x_ = 10 # Redeclare var - should warn but proceed
	x_ = 15 # Reassign - should work without warning
	x_
}

result = redeclareTest({})
~~~
# PROBLEMS
**DUPLICATE DEFINITION**
The name ``x_`` is being redeclared in this scope.

The redeclaration is here:
**can_var_scoping_var_redeclaration.md:6-2:7:**
```roc
	var x_ = 10 # Redeclare var - should warn but proceed
	x_ = 15 # Reassign - should work without warning
```

But ``x_`` was already defined here:
**can_var_scoping_var_redeclaration.md:5-2:6:**
```roc
	var x_ = 5
	var x_ = 10 # Redeclare var - should warn but proceed
```

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:60),
LowerIdent(4:1-4:14),OpAssign(4:15-4:16),OpBar(4:17-4:18),Underscore(4:18-4:19),OpBar(4:19-4:20),OpenCurly(4:21-4:22),Newline(1:1-1:1),
KwVar(5:2-5:5),LowerIdent(5:6-5:8),OpAssign(5:9-5:10),Int(5:11-5:12),Newline(1:1-1:1),
KwVar(6:2-6:5),LowerIdent(6:6-6:8),OpAssign(6:9-6:10),Int(6:11-6:13),Newline(6:15-6:55),
LowerIdent(7:2-7:4),OpAssign(7:5-7:6),Int(7:7-7:9),Newline(7:11-7:50),
LowerIdent(8:2-8:4),Newline(1:1-1:1),
CloseCurly(9:1-9:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(11:1-11:7),OpAssign(11:8-11:9),LowerIdent(11:10-11:23),NoSpaceOpenRound(11:23-11:24),OpenCurly(11:24-11:25),CloseCurly(11:25-11:26),CloseRound(11:26-11:27),EndOfFile(11:27-11:27),
~~~
# PARSE
~~~clojure
(file (1:1-11:27)
	(module (1:1-1:10) (exposes (1:8-1:10)))
	(statements
		(decl (4:1-9:2)
			(ident (4:1-4:14) "redeclareTest")
			(lambda (4:17-9:2)
				(args (underscore))
				(block (4:21-9:2)
					(statements
						(var (5:2-6:5)
							(name "x_")
							(int (5:11-5:12) "5"))
						(var (6:2-7:4)
							(name "x_")
							(int (6:11-6:13) "10"))
						(decl (7:2-7:9)
							(ident (7:2-7:4) "x_")
							(int (7:7-7:9) "15"))
						(ident (8:2-8:4) "" "x_")))))
		(decl (11:1-11:27)
			(ident (11:1-11:7) "result")
			(apply (11:10-11:27)
				(ident (11:10-11:23) "" "redeclareTest")
				(record (11:24-11:26))))))
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
			(p_assign (4:1-4:14)
				(pid 12)
				(ident "redeclareTest")))
		(def_expr
			(e_lambda (4:17-9:2)
				(args (p_underscore (4:18-4:19) (pid 13)))
				(e_block (4:21-9:2)
					(s_var (5:2-6:5)
						(pid 17)
						(p_assign (5:2-6:5)
							(pid 17)
							(ident "x_"))
						(e_int (5:11-5:12)
							(int_var 15)
							(precision_var 14)
							(literal "5")
							(value "TODO")
							(bound "u8")))
					(s_var (6:2-7:4)
						(pid 22)
						(p_assign (6:2-7:4)
							(pid 22)
							(ident "x_"))
						(e_int (6:11-6:13)
							(int_var 20)
							(precision_var 19)
							(literal "10")
							(value "TODO")
							(bound "u8")))
					(s_reassign (7:2-7:4)
						(pid 17)
						(e_int (7:7-7:9)
							(int_var 26)
							(precision_var 25)
							(literal "15")
							(value "TODO")
							(bound "u8")))
					(e_lookup (8:2-8:4) (pid 17))))))
	(d_let
		(def_pattern
			(p_assign (11:1-11:7)
				(pid 33)
				(ident "result")))
		(def_expr
			(e_call (11:10-11:27)
				(e_lookup (11:10-11:23) (pid 12))
				(e_runtime_error (1:1-1:1) "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "redeclareTest" 32 (type "*"))
		(def "result" 38 (type "*")))
	(expressions
		(expr (4:17-9:2) 31 (type "*"))
		(expr (11:10-11:27) 37 (type "*"))))
~~~