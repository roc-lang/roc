# META
~~~ini
description=Let-polymorphism with lists
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic empty list polymorphism
my_empty_list = []

# Empty list used in different contexts
int_list = [1, 2, 3]
str_list = ["hello", "world"]
float_list = [1.1, 2.2, 3.3]

# Append empty list (polymorphic use)
all_int_list = int_list ++ my_empty_list
all_str_list = str_list ++ my_empty_list
all_float_list = float_list ++ my_empty_list

# Function returning empty list
get_empty = |_| []

# Used at different types
empty_int_list = get_empty(42)
empty_str_list = get_empty("test")

main = |_| {
    # Type inference should work correctly
    len1 = List.len(all_int_list)
    len2 = List.len(all_str_list)
    len3 = List.len(all_float_list)
    len1 + len2 + len3
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_lists.md:12:26:12:27
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_lists.md:13:26:13:27
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_lists.md:14:30:14:31
INVALID STATEMENT - let_polymorphism_lists.md:12:28:12:41
INVALID STATEMENT - let_polymorphism_lists.md:13:28:13:41
INVALID STATEMENT - let_polymorphism_lists.md:14:32:14:45
UNDEFINED VARIABLE - let_polymorphism_lists.md:25:12:25:20
UNDEFINED VARIABLE - let_polymorphism_lists.md:26:12:26:20
UNDEFINED VARIABLE - let_polymorphism_lists.md:27:12:27:20
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **+** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**let_polymorphism_lists.md:12:26:12:27:**
```roc
all_int_list = int_list ++ my_empty_list
```
                         ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **+** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**let_polymorphism_lists.md:13:26:13:27:**
```roc
all_str_list = str_list ++ my_empty_list
```
                         ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **+** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**let_polymorphism_lists.md:14:30:14:31:**
```roc
all_float_list = float_list ++ my_empty_list
```
                             ^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**let_polymorphism_lists.md:12:28:12:41:**
```roc
all_int_list = int_list ++ my_empty_list
```
                           ^^^^^^^^^^^^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**let_polymorphism_lists.md:13:28:13:41:**
```roc
all_str_list = str_list ++ my_empty_list
```
                           ^^^^^^^^^^^^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**let_polymorphism_lists.md:14:32:14:45:**
```roc
all_float_list = float_list ++ my_empty_list
```
                               ^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `len` in this scope.
Is there an `import` or `exposing` missing up-top?

**let_polymorphism_lists.md:25:12:25:20:**
```roc
    len1 = List.len(all_int_list)
```
           ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `len` in this scope.
Is there an `import` or `exposing` missing up-top?

**let_polymorphism_lists.md:26:12:26:20:**
```roc
    len2 = List.len(all_str_list)
```
           ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `len` in this scope.
Is there an `import` or `exposing` missing up-top?

**let_polymorphism_lists.md:27:12:27:20:**
```roc
    len3 = List.len(all_float_list)
```
           ^^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),
LowerIdent(4:1-4:14),OpAssign(4:15-4:16),OpenSquare(4:17-4:18),CloseSquare(4:18-4:19),
LowerIdent(7:1-7:9),OpAssign(7:10-7:11),OpenSquare(7:12-7:13),Int(7:13-7:14),Comma(7:14-7:15),Int(7:16-7:17),Comma(7:17-7:18),Int(7:19-7:20),CloseSquare(7:20-7:21),
LowerIdent(8:1-8:9),OpAssign(8:10-8:11),OpenSquare(8:12-8:13),StringStart(8:13-8:14),StringPart(8:14-8:19),StringEnd(8:19-8:20),Comma(8:20-8:21),StringStart(8:22-8:23),StringPart(8:23-8:28),StringEnd(8:28-8:29),CloseSquare(8:29-8:30),
LowerIdent(9:1-9:11),OpAssign(9:12-9:13),OpenSquare(9:14-9:15),Float(9:15-9:18),Comma(9:18-9:19),Float(9:20-9:23),Comma(9:23-9:24),Float(9:25-9:28),CloseSquare(9:28-9:29),
LowerIdent(12:1-12:13),OpAssign(12:14-12:15),LowerIdent(12:16-12:24),OpPlus(12:25-12:26),OpPlus(12:26-12:27),LowerIdent(12:28-12:41),
LowerIdent(13:1-13:13),OpAssign(13:14-13:15),LowerIdent(13:16-13:24),OpPlus(13:25-13:26),OpPlus(13:26-13:27),LowerIdent(13:28-13:41),
LowerIdent(14:1-14:15),OpAssign(14:16-14:17),LowerIdent(14:18-14:28),OpPlus(14:29-14:30),OpPlus(14:30-14:31),LowerIdent(14:32-14:45),
LowerIdent(17:1-17:10),OpAssign(17:11-17:12),OpBar(17:13-17:14),Underscore(17:14-17:15),OpBar(17:15-17:16),OpenSquare(17:17-17:18),CloseSquare(17:18-17:19),
LowerIdent(20:1-20:15),OpAssign(20:16-20:17),LowerIdent(20:18-20:27),NoSpaceOpenRound(20:27-20:28),Int(20:28-20:30),CloseRound(20:30-20:31),
LowerIdent(21:1-21:15),OpAssign(21:16-21:17),LowerIdent(21:18-21:27),NoSpaceOpenRound(21:27-21:28),StringStart(21:28-21:29),StringPart(21:29-21:33),StringEnd(21:33-21:34),CloseRound(21:34-21:35),
LowerIdent(23:1-23:5),OpAssign(23:6-23:7),OpBar(23:8-23:9),Underscore(23:9-23:10),OpBar(23:10-23:11),OpenCurly(23:12-23:13),
LowerIdent(25:5-25:9),OpAssign(25:10-25:11),UpperIdent(25:12-25:16),NoSpaceDotLowerIdent(25:16-25:20),NoSpaceOpenRound(25:20-25:21),LowerIdent(25:21-25:33),CloseRound(25:33-25:34),
LowerIdent(26:5-26:9),OpAssign(26:10-26:11),UpperIdent(26:12-26:16),NoSpaceDotLowerIdent(26:16-26:20),NoSpaceOpenRound(26:20-26:21),LowerIdent(26:21-26:33),CloseRound(26:33-26:34),
LowerIdent(27:5-27:9),OpAssign(27:10-27:11),UpperIdent(27:12-27:16),NoSpaceDotLowerIdent(27:16-27:20),NoSpaceOpenRound(27:20-27:21),LowerIdent(27:21-27:35),CloseRound(27:35-27:36),
LowerIdent(28:5-28:9),OpPlus(28:10-28:11),LowerIdent(28:12-28:16),OpPlus(28:17-28:18),LowerIdent(28:19-28:23),
CloseCurly(29:1-29:2),EndOfFile(29:2-29:2),
~~~
# PARSE
~~~clojure
(file @1.1-29.2
	(app @1.1-1.56
		(provides @1.5-1.11
			(exposed-lower-ident @1.6-1.10 (text "main")))
		(record-field @1.14-1.54 (name "pf")
			(e-string @1.27-1.54
				(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))
		(packages @1.12-1.56
			(record-field @1.14-1.54 (name "pf")
				(e-string @1.27-1.54
					(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-decl @4.1-4.19
			(p-ident @4.1-4.14 (raw "my_empty_list"))
			(e-list @4.17-4.19))
		(s-decl @7.1-7.21
			(p-ident @7.1-7.9 (raw "int_list"))
			(e-list @7.12-7.21
				(e-int @7.13-7.14 (raw "1"))
				(e-int @7.16-7.17 (raw "2"))
				(e-int @7.19-7.20 (raw "3"))))
		(s-decl @8.1-8.30
			(p-ident @8.1-8.9 (raw "str_list"))
			(e-list @8.12-8.30
				(e-string @8.13-8.20
					(e-string-part @8.14-8.19 (raw "hello")))
				(e-string @8.22-8.29
					(e-string-part @8.23-8.28 (raw "world")))))
		(s-decl @9.1-9.29
			(p-ident @9.1-9.11 (raw "float_list"))
			(e-list @9.14-9.29
				(e-frac @9.15-9.18 (raw "1.1"))
				(e-frac @9.20-9.23 (raw "2.2"))
				(e-frac @9.25-9.28 (raw "3.3"))))
		(s-decl @12.1-12.27
			(p-ident @12.1-12.13 (raw "all_int_list"))
			(e-binop @12.16-12.27 (op "+")
				(e-ident @12.16-12.24 (raw "int_list"))
				(e-malformed @12.26-12.27 (reason "expr_unexpected_token"))))
		(e-ident @12.28-12.41 (raw "my_empty_list"))
		(s-decl @13.1-13.27
			(p-ident @13.1-13.13 (raw "all_str_list"))
			(e-binop @13.16-13.27 (op "+")
				(e-ident @13.16-13.24 (raw "str_list"))
				(e-malformed @13.26-13.27 (reason "expr_unexpected_token"))))
		(e-ident @13.28-13.41 (raw "my_empty_list"))
		(s-decl @14.1-14.31
			(p-ident @14.1-14.15 (raw "all_float_list"))
			(e-binop @14.18-14.31 (op "+")
				(e-ident @14.18-14.28 (raw "float_list"))
				(e-malformed @14.30-14.31 (reason "expr_unexpected_token"))))
		(e-ident @14.32-14.45 (raw "my_empty_list"))
		(s-decl @17.1-17.19
			(p-ident @17.1-17.10 (raw "get_empty"))
			(e-lambda @17.13-17.19
				(args
					(p-underscore))
				(e-list @17.17-17.19)))
		(s-decl @20.1-20.31
			(p-ident @20.1-20.15 (raw "empty_int_list"))
			(e-apply @20.18-20.31
				(e-ident @20.18-20.27 (raw "get_empty"))
				(e-int @20.28-20.30 (raw "42"))))
		(s-decl @21.1-21.35
			(p-ident @21.1-21.15 (raw "empty_str_list"))
			(e-apply @21.18-21.35
				(e-ident @21.18-21.27 (raw "get_empty"))
				(e-string @21.28-21.34
					(e-string-part @21.29-21.33 (raw "test")))))
		(s-decl @23.1-29.2
			(p-ident @23.1-23.5 (raw "main"))
			(e-lambda @23.8-29.2
				(args
					(p-underscore))
				(e-block @23.12-29.2
					(statements
						(s-decl @25.5-25.34
							(p-ident @25.5-25.9 (raw "len1"))
							(e-apply @25.12-25.34
								(e-ident @25.12-25.20 (raw "List.len"))
								(e-ident @25.21-25.33 (raw "all_int_list"))))
						(s-decl @26.5-26.34
							(p-ident @26.5-26.9 (raw "len2"))
							(e-apply @26.12-26.34
								(e-ident @26.12-26.20 (raw "List.len"))
								(e-ident @26.21-26.33 (raw "all_str_list"))))
						(s-decl @27.5-27.36
							(p-ident @27.5-27.9 (raw "len3"))
							(e-apply @27.12-27.36
								(e-ident @27.12-27.20 (raw "List.len"))
								(e-ident @27.21-27.35 (raw "all_float_list"))))
						(e-binop @28.5-28.23 (op "+")
							(e-ident @28.5-28.9 (raw "len1"))
							(e-binop @28.12-28.23 (op "+")
								(e-ident @28.12-28.16 (raw "len2"))
								(e-ident @28.19-28.23 (raw "len3"))))))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic empty list polymorphism
my_empty_list = []

# Empty list used in different contexts
int_list = [1, 2, 3]
str_list = ["hello", "world"]
float_list = [1.1, 2.2, 3.3]

# Append empty list (polymorphic use)
all_int_list = int_list + 
my_empty_list
all_str_list = str_list + 
my_empty_list
all_float_list = float_list + 
my_empty_list

# Function returning empty list
get_empty = |_| []

# Used at different types
empty_int_list = get_empty(42)
empty_str_list = get_empty("test")

main = |_| {
	# Type inference should work correctly
	len1 = List.len(all_int_list)
	len2 = List.len(all_str_list)
	len3 = List.len(all_float_list)
	len1 + len2 + len3
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.14 (ident "my_empty_list"))
		(e-empty_list @4.17-4.19))
	(d-let
		(p-assign @7.1-7.9 (ident "int_list"))
		(e-list @7.12-7.21
			(elems
				(e-int @7.13-7.14 (value "1"))
				(e-int @7.16-7.17 (value "2"))
				(e-int @7.19-7.20 (value "3")))))
	(d-let
		(p-assign @8.1-8.9 (ident "str_list"))
		(e-list @8.12-8.30
			(elems
				(e-string @8.13-8.20
					(e-literal @8.14-8.19 (string "hello")))
				(e-string @8.22-8.29
					(e-literal @8.23-8.28 (string "world"))))))
	(d-let
		(p-assign @9.1-9.11 (ident "float_list"))
		(e-list @9.14-9.29
			(elems
				(e-dec-small @9.15-9.18 (numerator "11") (denominator-power-of-ten "1") (value "1.1"))
				(e-dec-small @9.20-9.23 (numerator "22") (denominator-power-of-ten "1") (value "2.2"))
				(e-dec-small @9.25-9.28 (numerator "33") (denominator-power-of-ten "1") (value "3.3")))))
	(d-let
		(p-assign @12.1-12.13 (ident "all_int_list"))
		(e-binop @12.16-12.27 (op "add")
			(e-lookup-local @12.16-12.24
				(p-assign @7.1-7.9 (ident "int_list")))
			(e-runtime-error (tag "expr_not_canonicalized"))))
	(d-let
		(p-assign @13.1-13.13 (ident "all_str_list"))
		(e-binop @13.16-13.27 (op "add")
			(e-lookup-local @13.16-13.24
				(p-assign @8.1-8.9 (ident "str_list")))
			(e-runtime-error (tag "expr_not_canonicalized"))))
	(d-let
		(p-assign @14.1-14.15 (ident "all_float_list"))
		(e-binop @14.18-14.31 (op "add")
			(e-lookup-local @14.18-14.28
				(p-assign @9.1-9.11 (ident "float_list")))
			(e-runtime-error (tag "expr_not_canonicalized"))))
	(d-let
		(p-assign @17.1-17.10 (ident "get_empty"))
		(e-lambda @17.13-17.19
			(args
				(p-underscore @17.14-17.15))
			(e-empty_list @17.17-17.19)))
	(d-let
		(p-assign @20.1-20.15 (ident "empty_int_list"))
		(e-call @20.18-20.31
			(e-lookup-local @20.18-20.27
				(p-assign @17.1-17.10 (ident "get_empty")))
			(e-int @20.28-20.30 (value "42"))))
	(d-let
		(p-assign @21.1-21.15 (ident "empty_str_list"))
		(e-call @21.18-21.35
			(e-lookup-local @21.18-21.27
				(p-assign @17.1-17.10 (ident "get_empty")))
			(e-string @21.28-21.34
				(e-literal @21.29-21.33 (string "test")))))
	(d-let
		(p-assign @23.1-23.5 (ident "main"))
		(e-lambda @23.8-29.2
			(args
				(p-underscore @23.9-23.10))
			(e-block @23.12-29.2
				(s-let @25.5-25.34
					(p-assign @25.5-25.9 (ident "len1"))
					(e-call @25.12-25.34
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-lookup-local @25.21-25.33
							(p-assign @12.1-12.13 (ident "all_int_list")))))
				(s-let @26.5-26.34
					(p-assign @26.5-26.9 (ident "len2"))
					(e-call @26.12-26.34
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-lookup-local @26.21-26.33
							(p-assign @13.1-13.13 (ident "all_str_list")))))
				(s-let @27.5-27.36
					(p-assign @27.5-27.9 (ident "len3"))
					(e-call @27.12-27.36
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-lookup-local @27.21-27.35
							(p-assign @14.1-14.15 (ident "all_float_list")))))
				(e-binop @28.5-28.23 (op "add")
					(e-lookup-local @28.5-28.9
						(p-assign @25.5-25.9 (ident "len1")))
					(e-binop @28.12-28.23 (op "add")
						(e-lookup-local @28.12-28.16
							(p-assign @26.5-26.9 (ident "len2")))
						(e-lookup-local @28.19-28.23
							(p-assign @27.5-27.9 (ident "len3")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.14 (type "List(elem)"))
		(patt @7.1-7.9 (type "List(Num(size))"))
		(patt @8.1-8.9 (type "List(Str)"))
		(patt @9.1-9.11 (type "List(Frac(size))"))
		(patt @12.1-12.13 (type "a"))
		(patt @13.1-13.13 (type "b"))
		(patt @14.1-14.15 (type "c"))
		(patt @17.1-17.10 (type "arg -> List(elem)"))
		(patt @20.1-20.15 (type "List(elem)"))
		(patt @21.1-21.15 (type "List(elem)"))
		(patt @23.1-23.5 (type "arg2 -> ret")))
	(expressions
		(expr @4.17-4.19 (type "List(elem)"))
		(expr @7.12-7.21 (type "List(Num(size))"))
		(expr @8.12-8.30 (type "List(Str)"))
		(expr @9.14-9.29 (type "List(Frac(size))"))
		(expr @12.16-12.27 (type "a"))
		(expr @13.16-13.27 (type "b"))
		(expr @14.18-14.31 (type "c"))
		(expr @17.13-17.19 (type "arg -> List(elem)"))
		(expr @20.18-20.31 (type "List(elem)"))
		(expr @21.18-21.35 (type "List(elem)"))
		(expr @23.8-29.2 (type "arg2 -> ret"))))
~~~
