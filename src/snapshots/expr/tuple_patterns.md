# META
~~~ini
description=Tuple pattern matching tests
type=expr
~~~
# SOURCE
~~~roc
{

    # Simple tuple destructuring
    (x, y) = (1, 2)

    # Nested tuple patterns
    ((a, b), (c, d)) = ((10, 20), (30, 40))

    # Mixed patterns with literals
    (first, second, third) = (100, 42, 200)

    # Tuple with string and tag patterns
    (name, string, boolean) = ("Alice", "fixed", True)

    # Tuple with list pattern
    (list, hello) = ([1, 2, 3], "hello")

    {}
}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **= (** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tuple_patterns.md:4:12:4:15:**
```roc
    (x, y) = (1, 2)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= (** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tuple_patterns.md:7:22:7:25:**
```roc
    ((a, b), (c, d)) = ((10, 20), (30, 40))
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= (** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tuple_patterns.md:10:28:10:31:**
```roc
    (first, second, third) = (100, 42, 200)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= (** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tuple_patterns.md:13:29:13:32:**
```roc
    (name, string, boolean) = ("Alice", "fixed", True)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= (** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**tuple_patterns.md:16:19:16:22:**
```roc
    (list, hello) = ([1, 2, 3], "hello")
```


**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `y` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `a` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `b` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `c` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `d` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `first` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `second` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `third` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `name` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `string` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `boolean` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `list` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `hello` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:6-3:33),
OpenRound(4:5-4:6),LowerIdent(4:6-4:7),Comma(4:7-4:8),LowerIdent(4:9-4:10),CloseRound(4:10-4:11),OpAssign(4:12-4:13),OpenRound(4:14-4:15),Int(4:15-4:16),Comma(4:16-4:17),Int(4:18-4:19),CloseRound(4:19-4:20),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:6-6:28),
OpenRound(7:5-7:6),NoSpaceOpenRound(7:6-7:7),LowerIdent(7:7-7:8),Comma(7:8-7:9),LowerIdent(7:10-7:11),CloseRound(7:11-7:12),Comma(7:12-7:13),OpenRound(7:14-7:15),LowerIdent(7:15-7:16),Comma(7:16-7:17),LowerIdent(7:18-7:19),CloseRound(7:19-7:20),CloseRound(7:20-7:21),OpAssign(7:22-7:23),OpenRound(7:24-7:25),NoSpaceOpenRound(7:25-7:26),Int(7:26-7:28),Comma(7:28-7:29),Int(7:30-7:32),CloseRound(7:32-7:33),Comma(7:33-7:34),OpenRound(7:35-7:36),Int(7:36-7:38),Comma(7:38-7:39),Int(7:40-7:42),CloseRound(7:42-7:43),CloseRound(7:43-7:44),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(9:6-9:35),
OpenRound(10:5-10:6),LowerIdent(10:6-10:11),Comma(10:11-10:12),LowerIdent(10:13-10:19),Comma(10:19-10:20),LowerIdent(10:21-10:26),CloseRound(10:26-10:27),OpAssign(10:28-10:29),OpenRound(10:30-10:31),Int(10:31-10:34),Comma(10:34-10:35),Int(10:36-10:38),Comma(10:38-10:39),Int(10:40-10:43),CloseRound(10:43-10:44),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(12:6-12:41),
OpenRound(13:5-13:6),LowerIdent(13:6-13:10),Comma(13:10-13:11),LowerIdent(13:12-13:18),Comma(13:18-13:19),LowerIdent(13:20-13:27),CloseRound(13:27-13:28),OpAssign(13:29-13:30),OpenRound(13:31-13:32),StringStart(13:32-13:33),StringPart(13:33-13:38),StringEnd(13:38-13:39),Comma(13:39-13:40),StringStart(13:41-13:42),StringPart(13:42-13:47),StringEnd(13:47-13:48),Comma(13:48-13:49),UpperIdent(13:50-13:54),CloseRound(13:54-13:55),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(15:6-15:30),
OpenRound(16:5-16:6),LowerIdent(16:6-16:10),Comma(16:10-16:11),LowerIdent(16:12-16:17),CloseRound(16:17-16:18),OpAssign(16:19-16:20),OpenRound(16:21-16:22),OpenSquare(16:22-16:23),Int(16:23-16:24),Comma(16:24-16:25),Int(16:26-16:27),Comma(16:27-16:28),Int(16:29-16:30),CloseSquare(16:30-16:31),Comma(16:31-16:32),StringStart(16:33-16:34),StringPart(16:34-16:39),StringEnd(16:39-16:40),CloseRound(16:40-16:41),Newline(1:1-1:1),
Newline(1:1-1:1),
OpenCurly(18:5-18:6),CloseCurly(18:6-18:7),Newline(1:1-1:1),
CloseCurly(19:1-19:2),EndOfFile(19:2-19:2),
~~~
# PARSE
~~~clojure
(block (1:1-19:2)
	(statements
		(tuple (4:5-4:11)
			(ident (4:6-4:7) "" "x")
			(ident (4:9-4:10) "" "y"))
		(malformed_expr (4:12-4:15) "expr_unexpected_token")
		(tuple (4:14-4:20)
			(int (4:15-4:16) "1")
			(int (4:18-4:19) "2"))
		(tuple (7:5-7:21)
			(tuple (7:6-7:12)
				(ident (7:7-7:8) "" "a")
				(ident (7:10-7:11) "" "b"))
			(tuple (7:14-7:20)
				(ident (7:15-7:16) "" "c")
				(ident (7:18-7:19) "" "d")))
		(malformed_expr (7:22-7:25) "expr_unexpected_token")
		(tuple (7:24-7:44)
			(tuple (7:25-7:33)
				(int (7:26-7:28) "10")
				(int (7:30-7:32) "20"))
			(tuple (7:35-7:43)
				(int (7:36-7:38) "30")
				(int (7:40-7:42) "40")))
		(tuple (10:5-10:27)
			(ident (10:6-10:11) "" "first")
			(ident (10:13-10:19) "" "second")
			(ident (10:21-10:26) "" "third"))
		(malformed_expr (10:28-10:31) "expr_unexpected_token")
		(tuple (10:30-10:44)
			(int (10:31-10:34) "100")
			(int (10:36-10:38) "42")
			(int (10:40-10:43) "200"))
		(tuple (13:5-13:28)
			(ident (13:6-13:10) "" "name")
			(ident (13:12-13:18) "" "string")
			(ident (13:20-13:27) "" "boolean"))
		(malformed_expr (13:29-13:32) "expr_unexpected_token")
		(tuple (13:31-13:55)
			(string (13:32-13:39) (string_part (13:33-13:38) "Alice"))
			(string (13:41-13:48) (string_part (13:42-13:47) "fixed"))
			(tag (13:50-13:54) "True"))
		(tuple (16:5-16:18)
			(ident (16:6-16:10) "" "list")
			(ident (16:12-16:17) "" "hello"))
		(malformed_expr (16:19-16:22) "expr_unexpected_token")
		(tuple (16:21-16:41)
			(list (16:22-16:31)
				(int (16:23-16:24) "1")
				(int (16:26-16:27) "2")
				(int (16:29-16:30) "3"))
			(string (16:33-16:40) (string_part (16:34-16:39) "hello")))
		(record (18:5-18:7))))
~~~
# FORMATTED
~~~roc
{

	# Simple tuple destructuring
	(x, y)
	
	(1, 2)

	# Nested tuple patterns
	((a, b), (c, d))
	
	((10, 20), (30, 40))

	# Mixed patterns with literals
	(first, second, third)
	
	(100, 42, 200)

	# Tuple with string and tag patterns
	(name, string, boolean)
	
	("Alice", "fixed", True)

	# Tuple with list pattern
	(list, hello)
	
	([1, 2, 3], "hello")

	{}
}
~~~
# CANONICALIZE
~~~clojure
(e_block (1:1-19:2)
	(s_expr (4:5-4:13)
		(e_tuple (4:5-4:11)
			(tuple_var "#76")
			(elems
				(e_runtime_error (4:6-4:7) "ident_not_in_scope")
				(e_runtime_error (4:9-4:10) "ident_not_in_scope"))))
	(s_expr (4:14-7:6)
		(e_tuple (4:14-4:20)
			(tuple_var "#85")
			(elems
				(e_int (4:15-4:16)
					(int_var 80)
					(precision_var 79)
					(literal "1")
					(value "TODO")
					(bound "u8"))
				(e_int (4:18-4:19)
					(int_var 83)
					(precision_var 82)
					(literal "2")
					(value "TODO")
					(bound "u8")))))
	(s_expr (7:5-7:23)
		(e_tuple (7:5-7:21)
			(tuple_var "#100")
			(elems
				(e_tuple (7:6-7:12)
					(tuple_var "#92")
					(elems
						(e_runtime_error (7:7-7:8) "ident_not_in_scope")
						(e_runtime_error (7:10-7:11) "ident_not_in_scope")))
				(e_tuple (7:14-7:20)
					(tuple_var "#98")
					(elems
						(e_runtime_error (7:15-7:16) "ident_not_in_scope")
						(e_runtime_error (7:18-7:19) "ident_not_in_scope"))))))
	(s_expr (7:24-10:6)
		(e_tuple (7:24-7:44)
			(tuple_var "#119")
			(elems
				(e_tuple (7:25-7:33)
					(tuple_var "#109")
					(elems
						(e_int (7:26-7:28)
							(int_var 104)
							(precision_var 103)
							(literal "10")
							(value "TODO")
							(bound "u8"))
						(e_int (7:30-7:32)
							(int_var 107)
							(precision_var 106)
							(literal "20")
							(value "TODO")
							(bound "u8"))))
				(e_tuple (7:35-7:43)
					(tuple_var "#117")
					(elems
						(e_int (7:36-7:38)
							(int_var 112)
							(precision_var 111)
							(literal "30")
							(value "TODO")
							(bound "u8"))
						(e_int (7:40-7:42)
							(int_var 115)
							(precision_var 114)
							(literal "40")
							(value "TODO")
							(bound "u8")))))))
	(s_expr (10:5-10:29)
		(e_tuple (10:5-10:27)
			(tuple_var "#128")
			(elems
				(e_runtime_error (10:6-10:11) "ident_not_in_scope")
				(e_runtime_error (10:13-10:19) "ident_not_in_scope")
				(e_runtime_error (10:21-10:26) "ident_not_in_scope"))))
	(s_expr (10:30-13:6)
		(e_tuple (10:30-10:44)
			(tuple_var "#140")
			(elems
				(e_int (10:31-10:34)
					(int_var 132)
					(precision_var 131)
					(literal "100")
					(value "TODO")
					(bound "u8"))
				(e_int (10:36-10:38)
					(int_var 135)
					(precision_var 134)
					(literal "42")
					(value "TODO")
					(bound "u8"))
				(e_int (10:40-10:43)
					(int_var 138)
					(precision_var 137)
					(literal "200")
					(value "TODO")
					(bound "u8")))))
	(s_expr (13:5-13:30)
		(e_tuple (13:5-13:28)
			(tuple_var "#149")
			(elems
				(e_runtime_error (13:6-13:10) "ident_not_in_scope")
				(e_runtime_error (13:12-13:18) "ident_not_in_scope")
				(e_runtime_error (13:20-13:27) "ident_not_in_scope"))))
	(s_expr (13:31-16:6)
		(e_tuple (13:31-13:55)
			(tuple_var "#158")
			(elems
				(e_string (13:32-13:39) (e_literal (13:33-13:38) "Alice"))
				(e_string (13:41-13:48) (e_literal (13:42-13:47) "fixed"))
				(e_tag (13:50-13:54)
					(ext_var 0)
					(name "True")
					(args "TODO")))))
	(s_expr (16:5-16:20)
		(e_tuple (16:5-16:18)
			(tuple_var "#165")
			(elems
				(e_runtime_error (16:6-16:10) "ident_not_in_scope")
				(e_runtime_error (16:12-16:17) "ident_not_in_scope"))))
	(s_expr (16:21-18:6)
		(e_tuple (16:21-16:41)
			(tuple_var "#181")
			(elems
				(e_list (16:22-16:31)
					(elem_var 177)
					(elems
						(e_int (16:23-16:24)
							(int_var 169)
							(precision_var 168)
							(literal "1")
							(value "TODO")
							(bound "u8"))
						(e_int (16:26-16:27)
							(int_var 172)
							(precision_var 171)
							(literal "2")
							(value "TODO")
							(bound "u8"))
						(e_int (16:29-16:30)
							(int_var 175)
							(precision_var 174)
							(literal "3")
							(value "TODO")
							(bound "u8"))))
				(e_string (16:33-16:40) (e_literal (16:34-16:39) "hello")))))
	(e_runtime_error (1:1-1:1) "not_implemented"))
~~~
# TYPES
~~~clojure
(expr 186 (type "*"))
~~~