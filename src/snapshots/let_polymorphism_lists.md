# META
~~~ini
description=Let-polymorphism with lists
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# True let-polymorphism: identity function
id = |x| x

# id used with different types
int_id = id(42)
str_id = id("hello")
list_id = id([1, 2, 3])

# Polymorphic function to make single-element lists
make_list = |x| [x]

# make_list used with different types
int_list = make_list(42)
str_list = make_list("hello")
bool_list = make_list(True)

# Polymorphic pair constructor
pair = |a, b| { fst: a, snd: b }

# pair used with different types
int_pair = pair(1, 2)
str_pair = pair("hello", "world")
mixed_pair = pair(42, "answer")

# Polymorphic function that returns empty list
empty = |_| []

# empty used at different types (type determined by context)
empty_ints = empty(0)
empty_strs = empty(0)

# Using empty lists in context to show they're different types
ints_with_empty = if True then [1, 2, 3] else empty_ints
strs_with_empty = if True then ["a", "b"] else empty_strs

main = |_| {
    # Return values demonstrating polymorphic instantiation
    { int_id, str_id, list_id, int_list, str_list, int_pair, str_pair }
}
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**let_polymorphism_lists.md:35:32:35:34:**
```roc
ints_with_empty = if True then [1, 2, 3] else empty_ints
```
                               ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, 2** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**let_polymorphism_lists.md:35:34:35:37:**
```roc
ints_with_empty = if True then [1, 2, 3] else empty_ints
```
                                 ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, 3** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**let_polymorphism_lists.md:35:37:35:40:**
```roc
ints_with_empty = if True then [1, 2, 3] else empty_ints
```
                                    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] else** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**let_polymorphism_lists.md:35:40:35:46:**
```roc
ints_with_empty = if True then [1, 2, 3] else empty_ints
```
                                       ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **else empty_ints** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**let_polymorphism_lists.md:35:42:35:57:**
```roc
ints_with_empty = if True then [1, 2, 3] else empty_ints
```
                                         ^^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**let_polymorphism_lists.md:36:32:36:34:**
```roc
strs_with_empty = if True then ["a", "b"] else empty_strs
```
                               ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**let_polymorphism_lists.md:36:36:36:39:**
```roc
strs_with_empty = if True then ["a", "b"] else empty_strs
```
                                   ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] else** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**let_polymorphism_lists.md:36:41:36:47:**
```roc
strs_with_empty = if True then ["a", "b"] else empty_strs
```
                                        ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **else empty_strs** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**let_polymorphism_lists.md:36:43:36:58:**
```roc
strs_with_empty = if True then ["a", "b"] else empty_strs
```
                                          ^^^^^^^^^^^^^^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:43),
LowerIdent(4:1-4:3),OpAssign(4:4-4:5),OpBar(4:6-4:7),LowerIdent(4:7-4:8),OpBar(4:8-4:9),LowerIdent(4:10-4:11),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:2-6:31),
LowerIdent(7:1-7:7),OpAssign(7:8-7:9),LowerIdent(7:10-7:12),NoSpaceOpenRound(7:12-7:13),Int(7:13-7:15),CloseRound(7:15-7:16),Newline(1:1-1:1),
LowerIdent(8:1-8:7),OpAssign(8:8-8:9),LowerIdent(8:10-8:12),NoSpaceOpenRound(8:12-8:13),StringStart(8:13-8:14),StringPart(8:14-8:19),StringEnd(8:19-8:20),CloseRound(8:20-8:21),Newline(1:1-1:1),
LowerIdent(9:1-9:8),OpAssign(9:9-9:10),LowerIdent(9:11-9:13),NoSpaceOpenRound(9:13-9:14),OpenSquare(9:14-9:15),Int(9:15-9:16),Comma(9:16-9:17),Int(9:18-9:19),Comma(9:19-9:20),Int(9:21-9:22),CloseSquare(9:22-9:23),CloseRound(9:23-9:24),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(11:2-11:52),
LowerIdent(12:1-12:10),OpAssign(12:11-12:12),OpBar(12:13-12:14),LowerIdent(12:14-12:15),OpBar(12:15-12:16),OpenSquare(12:17-12:18),LowerIdent(12:18-12:19),CloseSquare(12:19-12:20),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(14:2-14:38),
LowerIdent(15:1-15:9),OpAssign(15:10-15:11),LowerIdent(15:12-15:21),NoSpaceOpenRound(15:21-15:22),Int(15:22-15:24),CloseRound(15:24-15:25),Newline(1:1-1:1),
LowerIdent(16:1-16:9),OpAssign(16:10-16:11),LowerIdent(16:12-16:21),NoSpaceOpenRound(16:21-16:22),StringStart(16:22-16:23),StringPart(16:23-16:28),StringEnd(16:28-16:29),CloseRound(16:29-16:30),Newline(1:1-1:1),
LowerIdent(17:1-17:10),OpAssign(17:11-17:12),LowerIdent(17:13-17:22),NoSpaceOpenRound(17:22-17:23),UpperIdent(17:23-17:27),CloseRound(17:27-17:28),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(19:2-19:31),
LowerIdent(20:1-20:5),OpAssign(20:6-20:7),OpBar(20:8-20:9),LowerIdent(20:9-20:10),Comma(20:10-20:11),LowerIdent(20:12-20:13),OpBar(20:13-20:14),OpenCurly(20:15-20:16),LowerIdent(20:17-20:20),OpColon(20:20-20:21),LowerIdent(20:22-20:23),Comma(20:23-20:24),LowerIdent(20:25-20:28),OpColon(20:28-20:29),LowerIdent(20:30-20:31),CloseCurly(20:32-20:33),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(22:2-22:33),
LowerIdent(23:1-23:9),OpAssign(23:10-23:11),LowerIdent(23:12-23:16),NoSpaceOpenRound(23:16-23:17),Int(23:17-23:18),Comma(23:18-23:19),Int(23:20-23:21),CloseRound(23:21-23:22),Newline(1:1-1:1),
LowerIdent(24:1-24:9),OpAssign(24:10-24:11),LowerIdent(24:12-24:16),NoSpaceOpenRound(24:16-24:17),StringStart(24:17-24:18),StringPart(24:18-24:23),StringEnd(24:23-24:24),Comma(24:24-24:25),StringStart(24:26-24:27),StringPart(24:27-24:32),StringEnd(24:32-24:33),CloseRound(24:33-24:34),Newline(1:1-1:1),
LowerIdent(25:1-25:11),OpAssign(25:12-25:13),LowerIdent(25:14-25:18),NoSpaceOpenRound(25:18-25:19),Int(25:19-25:21),Comma(25:21-25:22),StringStart(25:23-25:24),StringPart(25:24-25:30),StringEnd(25:30-25:31),CloseRound(25:31-25:32),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(27:2-27:47),
LowerIdent(28:1-28:6),OpAssign(28:7-28:8),OpBar(28:9-28:10),Underscore(28:10-28:11),OpBar(28:11-28:12),OpenSquare(28:13-28:14),CloseSquare(28:14-28:15),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(30:2-30:61),
LowerIdent(31:1-31:11),OpAssign(31:12-31:13),LowerIdent(31:14-31:19),NoSpaceOpenRound(31:19-31:20),Int(31:20-31:21),CloseRound(31:21-31:22),Newline(1:1-1:1),
LowerIdent(32:1-32:11),OpAssign(32:12-32:13),LowerIdent(32:14-32:19),NoSpaceOpenRound(32:19-32:20),Int(32:20-32:21),CloseRound(32:21-32:22),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(34:2-34:63),
LowerIdent(35:1-35:16),OpAssign(35:17-35:18),KwIf(35:19-35:21),UpperIdent(35:22-35:26),LowerIdent(35:27-35:31),OpenSquare(35:32-35:33),Int(35:33-35:34),Comma(35:34-35:35),Int(35:36-35:37),Comma(35:37-35:38),Int(35:39-35:40),CloseSquare(35:40-35:41),KwElse(35:42-35:46),LowerIdent(35:47-35:57),Newline(1:1-1:1),
LowerIdent(36:1-36:16),OpAssign(36:17-36:18),KwIf(36:19-36:21),UpperIdent(36:22-36:26),LowerIdent(36:27-36:31),OpenSquare(36:32-36:33),StringStart(36:33-36:34),StringPart(36:34-36:35),StringEnd(36:35-36:36),Comma(36:36-36:37),StringStart(36:38-36:39),StringPart(36:39-36:40),StringEnd(36:40-36:41),CloseSquare(36:41-36:42),KwElse(36:43-36:47),LowerIdent(36:48-36:58),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(38:1-38:5),OpAssign(38:6-38:7),OpBar(38:8-38:9),Underscore(38:9-38:10),OpBar(38:10-38:11),OpenCurly(38:12-38:13),Newline(1:1-1:1),
Newline(39:6-39:60),
OpenCurly(40:5-40:6),LowerIdent(40:7-40:13),Comma(40:13-40:14),LowerIdent(40:15-40:21),Comma(40:21-40:22),LowerIdent(40:23-40:30),Comma(40:30-40:31),LowerIdent(40:32-40:40),Comma(40:40-40:41),LowerIdent(40:42-40:50),Comma(40:50-40:51),LowerIdent(40:52-40:60),Comma(40:60-40:61),LowerIdent(40:62-40:70),CloseCurly(40:71-40:72),Newline(1:1-1:1),
CloseCurly(41:1-41:2),EndOfFile(41:2-41:2),
~~~
# PARSE
~~~clojure
(file @1.1-41.2
	(app @1.1-1.56
		(provides @1.6-1.11
			(exposed-lower-ident (text "main")))
		(record-field @1.14-1.56 (name "pf")
			(e-string @1.27-1.54
				(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))
		(packages @1.12-1.56
			(record-field @1.14-1.56 (name "pf")
				(e-string @1.27-1.54
					(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-decl @4.1-4.11
			(p-ident @4.1-4.3 (raw "id"))
			(e-lambda @4.6-4.11
				(args
					(p-ident @4.7-4.8 (raw "x")))
				(e-ident @4.10-4.11 (raw "x"))))
		(s-decl @7.1-7.16
			(p-ident @7.1-7.7 (raw "int_id"))
			(e-apply @7.10-7.16
				(e-ident @7.10-7.12 (raw "id"))
				(e-int @7.13-7.15 (raw "42"))))
		(s-decl @8.1-8.21
			(p-ident @8.1-8.7 (raw "str_id"))
			(e-apply @8.10-8.21
				(e-ident @8.10-8.12 (raw "id"))
				(e-string @8.13-8.20
					(e-string-part @8.14-8.19 (raw "hello")))))
		(s-decl @9.1-9.24
			(p-ident @9.1-9.8 (raw "list_id"))
			(e-apply @9.11-9.24
				(e-ident @9.11-9.13 (raw "id"))
				(e-list @9.14-9.23
					(e-int @9.15-9.16 (raw "1"))
					(e-int @9.18-9.19 (raw "2"))
					(e-int @9.21-9.22 (raw "3")))))
		(s-decl @12.1-12.20
			(p-ident @12.1-12.10 (raw "make_list"))
			(e-lambda @12.13-12.20
				(args
					(p-ident @12.14-12.15 (raw "x")))
				(e-list @12.17-12.20
					(e-ident @12.18-12.19 (raw "x")))))
		(s-decl @15.1-15.25
			(p-ident @15.1-15.9 (raw "int_list"))
			(e-apply @15.12-15.25
				(e-ident @15.12-15.21 (raw "make_list"))
				(e-int @15.22-15.24 (raw "42"))))
		(s-decl @16.1-16.30
			(p-ident @16.1-16.9 (raw "str_list"))
			(e-apply @16.12-16.30
				(e-ident @16.12-16.21 (raw "make_list"))
				(e-string @16.22-16.29
					(e-string-part @16.23-16.28 (raw "hello")))))
		(s-decl @17.1-17.28
			(p-ident @17.1-17.10 (raw "bool_list"))
			(e-apply @17.13-17.28
				(e-ident @17.13-17.22 (raw "make_list"))
				(e-tag @17.23-17.27 (raw "True"))))
		(s-decl @20.1-20.33
			(p-ident @20.1-20.5 (raw "pair"))
			(e-lambda @20.8-20.33
				(args
					(p-ident @20.9-20.10 (raw "a"))
					(p-ident @20.12-20.13 (raw "b")))
				(e-record @20.15-20.33
					(field (field "fst") (optional false)
						(e-ident @20.22-20.23 (raw "a")))
					(field (field "snd") (optional false)
						(e-ident @20.30-20.31 (raw "b"))))))
		(s-decl @23.1-23.22
			(p-ident @23.1-23.9 (raw "int_pair"))
			(e-apply @23.12-23.22
				(e-ident @23.12-23.16 (raw "pair"))
				(e-int @23.17-23.18 (raw "1"))
				(e-int @23.20-23.21 (raw "2"))))
		(s-decl @24.1-24.34
			(p-ident @24.1-24.9 (raw "str_pair"))
			(e-apply @24.12-24.34
				(e-ident @24.12-24.16 (raw "pair"))
				(e-string @24.17-24.24
					(e-string-part @24.18-24.23 (raw "hello")))
				(e-string @24.26-24.33
					(e-string-part @24.27-24.32 (raw "world")))))
		(s-decl @25.1-25.32
			(p-ident @25.1-25.11 (raw "mixed_pair"))
			(e-apply @25.14-25.32
				(e-ident @25.14-25.18 (raw "pair"))
				(e-int @25.19-25.21 (raw "42"))
				(e-string @25.23-25.31
					(e-string-part @25.24-25.30 (raw "answer")))))
		(s-decl @28.1-28.15
			(p-ident @28.1-28.6 (raw "empty"))
			(e-lambda @28.9-28.15
				(args
					(p-underscore))
				(e-list @28.13-28.15)))
		(s-decl @31.1-31.22
			(p-ident @31.1-31.11 (raw "empty_ints"))
			(e-apply @31.14-31.22
				(e-ident @31.14-31.19 (raw "empty"))
				(e-int @31.20-31.21 (raw "0"))))
		(s-decl @32.1-32.22
			(p-ident @32.1-32.11 (raw "empty_strs"))
			(e-apply @32.14-32.22
				(e-ident @32.14-32.19 (raw "empty"))
				(e-int @32.20-32.21 (raw "0"))))
		(s-decl @35.1-35.34
			(p-ident @35.1-35.16 (raw "ints_with_empty"))
			(e-malformed @35.32-35.34 (reason "no_else")))
		(e-int @35.33-35.34 (raw "1"))
		(e-malformed @35.34-35.37 (reason "expr_unexpected_token"))
		(e-int @35.36-35.37 (raw "2"))
		(e-malformed @35.37-35.40 (reason "expr_unexpected_token"))
		(e-int @35.39-35.40 (raw "3"))
		(e-malformed @35.40-35.46 (reason "expr_unexpected_token"))
		(e-malformed @35.42-35.57 (reason "expr_unexpected_token"))
		(e-ident @35.47-35.57 (raw "empty_ints"))
		(s-decl @36.1-36.34
			(p-ident @36.1-36.16 (raw "strs_with_empty"))
			(e-malformed @36.32-36.34 (reason "no_else")))
		(e-string @36.33-36.36
			(e-string-part @36.34-36.35 (raw "a")))
		(e-malformed @36.36-36.39 (reason "expr_unexpected_token"))
		(e-string @36.38-36.41
			(e-string-part @36.39-36.40 (raw "b")))
		(e-malformed @36.41-36.47 (reason "expr_unexpected_token"))
		(e-malformed @36.43-36.58 (reason "expr_unexpected_token"))
		(e-ident @36.48-36.58 (raw "empty_strs"))
		(s-decl @38.1-41.2
			(p-ident @38.1-38.5 (raw "main"))
			(e-lambda @38.8-41.2
				(args
					(p-underscore))
				(e-block @38.12-41.2
					(statements
						(e-record @40.5-40.72
							(field (field "int_id") (optional false))
							(field (field "str_id") (optional false))
							(field (field "list_id") (optional false))
							(field (field "int_list") (optional false))
							(field (field "str_list") (optional false))
							(field (field "int_pair") (optional false))
							(field (field "str_pair") (optional false)))))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# True let-polymorphism: identity function
id = |x| x

# id used with different types
int_id = id(42)
str_id = id("hello")
list_id = id([1, 2, 3])

# Polymorphic function to make single-element lists
make_list = |x| [x]

# make_list used with different types
int_list = make_list(42)
str_list = make_list("hello")
bool_list = make_list(True)

# Polymorphic pair constructor
pair = |a, b| {fst: a, snd: b}

# pair used with different types
int_pair = pair(1, 2)
str_pair = pair("hello", "world")
mixed_pair = pair(42, "answer")

# Polymorphic function that returns empty list
empty = |_| []

# empty used at different types (type determined by context)
empty_ints = empty(0)
empty_strs = empty(0)

# Using empty lists in context to show they're different types
ints_with_empty = 123empty_ints
strs_with_empty = "a""b"empty_strs

main = |_| {
	# Return values demonstrating polymorphic instantiation
	{int_id, str_id, list_id, int_list, str_list, int_pair, str_pair}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.3 (ident "id"))
		(e-lambda @4.6-4.11
			(args
				(p-assign @4.7-4.8 (ident "x")))
			(e-lookup-local @4.10-4.11
				(pattern @4.7-4.8))))
	(d-let
		(p-assign @7.1-7.7 (ident "int_id"))
		(e-call @7.10-7.16
			(e-lookup-local @7.10-7.12
				(pattern @4.1-4.3))
			(e-int @7.13-7.15 (value "42"))))
	(d-let
		(p-assign @8.1-8.7 (ident "str_id"))
		(e-call @8.10-8.21
			(e-lookup-local @8.10-8.12
				(pattern @4.1-4.3))
			(e-string @8.13-8.20
				(e-literal @8.14-8.19 (string "hello")))))
	(d-let
		(p-assign @9.1-9.8 (ident "list_id"))
		(e-call @9.11-9.24
			(e-lookup-local @9.11-9.13
				(pattern @4.1-4.3))
			(e-list @9.14-9.23
				(elems
					(e-int @9.15-9.16 (value "1"))
					(e-int @9.18-9.19 (value "2"))
					(e-int @9.21-9.22 (value "3"))))))
	(d-let
		(p-assign @12.1-12.10 (ident "make_list"))
		(e-lambda @12.13-12.20
			(args
				(p-assign @12.14-12.15 (ident "x")))
			(e-list @12.17-12.20
				(elems
					(e-lookup-local @12.18-12.19
						(pattern @12.14-12.15))))))
	(d-let
		(p-assign @15.1-15.9 (ident "int_list"))
		(e-call @15.12-15.25
			(e-lookup-local @15.12-15.21
				(pattern @12.1-12.10))
			(e-int @15.22-15.24 (value "42"))))
	(d-let
		(p-assign @16.1-16.9 (ident "str_list"))
		(e-call @16.12-16.30
			(e-lookup-local @16.12-16.21
				(pattern @12.1-12.10))
			(e-string @16.22-16.29
				(e-literal @16.23-16.28 (string "hello")))))
	(d-let
		(p-assign @17.1-17.10 (ident "bool_list"))
		(e-call @17.13-17.28
			(e-lookup-local @17.13-17.22
				(pattern @12.1-12.10))
			(e-tag @17.23-17.27 (name "True"))))
	(d-let
		(p-assign @20.1-20.5 (ident "pair"))
		(e-lambda @20.8-20.33
			(args
				(p-assign @20.9-20.10 (ident "a"))
				(p-assign @20.12-20.13 (ident "b")))
			(e-record @20.15-20.33
				(fields
					(field (name "fst")
						(e-lookup-local @20.22-20.23
							(pattern @20.9-20.10)))
					(field (name "snd")
						(e-lookup-local @20.30-20.31
							(pattern @20.12-20.13)))))))
	(d-let
		(p-assign @23.1-23.9 (ident "int_pair"))
		(e-call @23.12-23.22
			(e-lookup-local @23.12-23.16
				(pattern @20.1-20.5))
			(e-int @23.17-23.18 (value "1"))
			(e-int @23.20-23.21 (value "2"))))
	(d-let
		(p-assign @24.1-24.9 (ident "str_pair"))
		(e-call @24.12-24.34
			(e-lookup-local @24.12-24.16
				(pattern @20.1-20.5))
			(e-string @24.17-24.24
				(e-literal @24.18-24.23 (string "hello")))
			(e-string @24.26-24.33
				(e-literal @24.27-24.32 (string "world")))))
	(d-let
		(p-assign @25.1-25.11 (ident "mixed_pair"))
		(e-call @25.14-25.32
			(e-lookup-local @25.14-25.18
				(pattern @20.1-20.5))
			(e-int @25.19-25.21 (value "42"))
			(e-string @25.23-25.31
				(e-literal @25.24-25.30 (string "answer")))))
	(d-let
		(p-assign @28.1-28.6 (ident "empty"))
		(e-lambda @28.9-28.15
			(args
				(p-underscore @28.10-28.11))
			(e-empty_list @28.13-28.15)))
	(d-let
		(p-assign @31.1-31.11 (ident "empty_ints"))
		(e-call @31.14-31.22
			(e-lookup-local @31.14-31.19
				(pattern @28.1-28.6))
			(e-int @31.20-31.21 (value "0"))))
	(d-let
		(p-assign @32.1-32.11 (ident "empty_strs"))
		(e-call @32.14-32.22
			(e-lookup-local @32.14-32.19
				(pattern @28.1-28.6))
			(e-int @32.20-32.21 (value "0"))))
	(d-let
		(p-assign @35.1-35.16 (ident "ints_with_empty"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign @36.1-36.16 (ident "strs_with_empty"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign @38.1-38.5 (ident "main"))
		(e-lambda @38.8-41.2
			(args
				(p-underscore @38.9-38.10))
			(e-block @38.12-41.2
				(e-record @40.5-40.72
					(fields
						(field (name "int_id")
							(e-lookup-local @40.7-40.14
								(pattern @7.1-7.7)))
						(field (name "str_id")
							(e-lookup-local @40.15-40.22
								(pattern @8.1-8.7)))
						(field (name "list_id")
							(e-lookup-local @40.23-40.31
								(pattern @9.1-9.8)))
						(field (name "int_list")
							(e-lookup-local @40.32-40.41
								(pattern @15.1-15.9)))
						(field (name "str_list")
							(e-lookup-local @40.42-40.51
								(pattern @16.1-16.9)))
						(field (name "int_pair")
							(e-lookup-local @40.52-40.61
								(pattern @23.1-23.9)))
						(field (name "str_pair")
							(e-lookup-local @40.62-40.72
								(pattern @24.1-24.9)))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.3 (type "* -> *"))
		(patt @7.1-7.7 (type "*"))
		(patt @8.1-8.7 (type "*"))
		(patt @9.1-9.8 (type "*"))
		(patt @12.1-12.10 (type "* -> List(*)"))
		(patt @15.1-15.9 (type "List(*)"))
		(patt @16.1-16.9 (type "List(*)"))
		(patt @17.1-17.10 (type "List(*)"))
		(patt @20.1-20.5 (type "*, * -> { fst: *, snd: * }"))
		(patt @23.1-23.9 (type "{ fst: *, snd: * }"))
		(patt @24.1-24.9 (type "{ fst: *, snd: * }"))
		(patt @25.1-25.11 (type "{ fst: *, snd: * }"))
		(patt @28.1-28.6 (type "* -> List(*)"))
		(patt @31.1-31.11 (type "List(*)"))
		(patt @32.1-32.11 (type "List(*)"))
		(patt @35.1-35.16 (type "Error"))
		(patt @36.1-36.16 (type "Error"))
		(patt @38.1-38.5 (type "* -> { int_id: *, str_id: *, list_id: *, int_list: List(*), str_list: List(*), int_pair: { fst: *, snd: * }, str_pair: { fst: *, snd: * } }")))
	(expressions
		(expr @4.6-4.11 (type "* -> *"))
		(expr @7.10-7.16 (type "*"))
		(expr @8.10-8.21 (type "*"))
		(expr @9.11-9.24 (type "*"))
		(expr @12.13-12.20 (type "* -> List(*)"))
		(expr @15.12-15.25 (type "List(*)"))
		(expr @16.12-16.30 (type "List(*)"))
		(expr @17.13-17.28 (type "List(*)"))
		(expr @20.8-20.33 (type "*, * -> { fst: *, snd: * }"))
		(expr @23.12-23.22 (type "{ fst: *, snd: * }"))
		(expr @24.12-24.34 (type "{ fst: *, snd: * }"))
		(expr @25.14-25.32 (type "{ fst: *, snd: * }"))
		(expr @28.9-28.15 (type "* -> List(*)"))
		(expr @31.14-31.22 (type "List(*)"))
		(expr @32.14-32.22 (type "List(*)"))
		(expr @35.32-35.34 (type "Error"))
		(expr @36.32-36.34 (type "Error"))
		(expr @38.8-41.2 (type "* -> { int_id: *, str_id: *, list_id: *, int_list: List(*), str_list: List(*), int_pair: { fst: *, snd: * }, str_pair: { fst: *, snd: * } }"))))
~~~
