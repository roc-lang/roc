# META
~~~ini
description=Dot access with proper variable definitions
type=expr
~~~
# SOURCE
~~~roc
{
    list = [1, 2, 3]
    fn = |x| x + 1
    list.map(fn)
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:9),OpAssign(2:10-2:11),OpenSquare(2:12-2:13),Int(2:13-2:14),Comma(2:14-2:15),Int(2:16-2:17),Comma(2:17-2:18),Int(2:19-2:20),CloseSquare(2:20-2:21),Newline(1:1-1:1),
LowerIdent(3:5-3:7),OpAssign(3:8-3:9),OpBar(3:10-3:11),LowerIdent(3:11-3:12),OpBar(3:12-3:13),LowerIdent(3:14-3:15),OpPlus(3:16-3:17),Int(3:18-3:19),Newline(1:1-1:1),
LowerIdent(4:5-4:9),NoSpaceDotLowerIdent(4:9-4:13),NoSpaceOpenRound(4:13-4:14),LowerIdent(4:14-4:16),CloseRound(4:16-4:17),Newline(1:1-1:1),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(block (1:1-5:2)
	(statements
		(decl (2:5-2:21)
			(ident (2:5-2:9) "list")
			(list (2:12-2:21)
				(int (2:13-2:14) "1")
				(int (2:16-2:17) "2")
				(int (2:19-2:20) "3")))
		(decl (3:5-4:9)
			(ident (3:5-3:7) "fn")
			(lambda (3:10-4:9)
				(args (ident (3:11-3:12) "x"))
				(binop (3:14-4:9)
					"+"
					(ident (3:14-3:15) "" "x")
					(int (3:18-3:19) "1"))))
		(field_access (4:5-5:2)
			(binop (4:5-5:2)
				"{"
				(ident (4:5-4:9) "" "list")
				(apply (4:9-4:17)
					(ident (4:9-4:13) "" ".map")
					(ident (4:14-4:16) "" "fn"))))))
~~~
# FORMATTED
~~~roc
{
	list = [1, 2, 3]
	fn = |x| x + 1
	list.map(fn)
}
~~~
# CANONICALIZE
~~~clojure
(e_block (1:1-5:2)
	(s_let (2:5-2:21)
		(p_assign (2:5-2:9)
			(pid 72)
			(ident "list"))
		(e_list (2:12-2:21)
			(elem_var 82)
			(elems
				(e_int (2:13-2:14)
					(int_var 74)
					(precision_var 73)
					(literal "1")
					(value "TODO")
					(bound "u8"))
				(e_int (2:16-2:17)
					(int_var 77)
					(precision_var 76)
					(literal "2")
					(value "TODO")
					(bound "u8"))
				(e_int (2:19-2:20)
					(int_var 80)
					(precision_var 79)
					(literal "3")
					(value "TODO")
					(bound "u8")))))
	(s_let (3:5-4:9)
		(p_assign (3:5-3:7)
			(pid 85)
			(ident "fn"))
		(e_lambda (3:10-4:9)
			(args
				(p_assign (3:11-3:12)
					(pid 86)
					(ident "x")))
			(e_binop (3:14-4:9)
				"add"
				(e_lookup (3:14-3:15) (pid 86))
				(e_int (3:18-3:19)
					(int_var 89)
					(precision_var 88)
					(literal "1")
					(value "TODO")
					(bound "u8")))))
	(e_dot_access (4:5-5:2)
		(e_lookup (4:5-4:9) (pid 72))
		"map"
		(e_lookup (4:14-4:16) (pid 85))))
~~~
# TYPES
~~~clojure
(expr 97 (type "*"))
~~~