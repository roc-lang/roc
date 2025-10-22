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
PARSE ERROR - let_polymorphism_lists.md:12:28:12:41
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_lists.md:13:26:13:27
PARSE ERROR - let_polymorphism_lists.md:13:28:13:41
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_lists.md:14:30:14:31
PARSE ERROR - let_polymorphism_lists.md:14:32:14:45
UNRECOGNIZED SYNTAX - let_polymorphism_lists.md:12:16:12:27
UNRECOGNIZED SYNTAX - let_polymorphism_lists.md:13:16:13:27
UNRECOGNIZED SYNTAX - let_polymorphism_lists.md:14:18:14:31
UNDEFINED VARIABLE - let_polymorphism_lists.md:25:12:25:20
UNDEFINED VARIABLE - let_polymorphism_lists.md:26:12:26:20
UNDEFINED VARIABLE - let_polymorphism_lists.md:27:12:27:20
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **+** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**let_polymorphism_lists.md:12:26:12:27:**
```roc
all_int_list = int_list ++ my_empty_list
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**let_polymorphism_lists.md:12:28:12:41:**
```roc
all_int_list = int_list ++ my_empty_list
```
                           ^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **+** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**let_polymorphism_lists.md:13:26:13:27:**
```roc
all_str_list = str_list ++ my_empty_list
```
                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**let_polymorphism_lists.md:13:28:13:41:**
```roc
all_str_list = str_list ++ my_empty_list
```
                           ^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **+** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**let_polymorphism_lists.md:14:30:14:31:**
```roc
all_float_list = float_list ++ my_empty_list
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**let_polymorphism_lists.md:14:32:14:45:**
```roc
all_float_list = float_list ++ my_empty_list
```
                               ^^^^^^^^^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**let_polymorphism_lists.md:12:16:12:27:**
```roc
all_int_list = int_list ++ my_empty_list
```
               ^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**let_polymorphism_lists.md:13:16:13:27:**
```roc
all_str_list = str_list ++ my_empty_list
```
               ^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**let_polymorphism_lists.md:14:18:14:31:**
```roc
all_float_list = float_list ++ my_empty_list
```
                 ^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

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
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpenSquare,CloseSquare,
LowerIdent,OpAssign,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
LowerIdent,OpAssign,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,
LowerIdent,OpAssign,OpenSquare,Float,Comma,Float,Comma,Float,CloseSquare,
LowerIdent,OpAssign,LowerIdent,OpPlus,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,OpPlus,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,OpPlus,OpPlus,LowerIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenSquare,CloseSquare,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-decl
			(p-ident (raw "my_empty_list"))
			(e-list))
		(s-decl
			(p-ident (raw "int_list"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))
				(e-int (raw "3"))))
		(s-decl
			(p-ident (raw "str_list"))
			(e-list
				(e-string
					(e-string-part (raw "hello")))
				(e-string
					(e-string-part (raw "world")))))
		(s-decl
			(p-ident (raw "float_list"))
			(e-list
				(e-frac (raw "1.1"))
				(e-frac (raw "2.2"))
				(e-frac (raw "3.3"))))
		(s-decl
			(p-ident (raw "all_int_list"))
			(e-binop (op "+")
				(e-ident (raw "int_list"))
				(e-malformed (reason "expr_unexpected_token"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "all_str_list"))
			(e-binop (op "+")
				(e-ident (raw "str_list"))
				(e-malformed (reason "expr_unexpected_token"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "all_float_list"))
			(e-binop (op "+")
				(e-ident (raw "float_list"))
				(e-malformed (reason "expr_unexpected_token"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "get_empty"))
			(e-lambda
				(args
					(p-underscore))
				(e-list)))
		(s-decl
			(p-ident (raw "empty_int_list"))
			(e-apply
				(e-ident (raw "get_empty"))
				(e-int (raw "42"))))
		(s-decl
			(p-ident (raw "empty_str_list"))
			(e-apply
				(e-ident (raw "get_empty"))
				(e-string
					(e-string-part (raw "test")))))
		(s-decl
			(p-ident (raw "main"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "len1"))
							(e-apply
								(e-ident (raw "List.len"))
								(e-ident (raw "all_int_list"))))
						(s-decl
							(p-ident (raw "len2"))
							(e-apply
								(e-ident (raw "List.len"))
								(e-ident (raw "all_str_list"))))
						(s-decl
							(p-ident (raw "len3"))
							(e-apply
								(e-ident (raw "List.len"))
								(e-ident (raw "all_float_list"))))
						(e-binop (op "+")
							(e-binop (op "+")
								(e-ident (raw "len1"))
								(e-ident (raw "len2")))
							(e-ident (raw "len3")))))))))
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

all_str_list = str_list + 

all_float_list = float_list + 


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
		(p-assign (ident "my_empty_list"))
		(e-empty_list))
	(d-let
		(p-assign (ident "int_list"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2"))
				(e-num (value "3")))))
	(d-let
		(p-assign (ident "str_list"))
		(e-list
			(elems
				(e-string
					(e-literal (string "hello")))
				(e-string
					(e-literal (string "world"))))))
	(d-let
		(p-assign (ident "float_list"))
		(e-list
			(elems
				(e-dec-small (numerator "11") (denominator-power-of-ten "1") (value "1.1"))
				(e-dec-small (numerator "22") (denominator-power-of-ten "1") (value "2.2"))
				(e-dec-small (numerator "33") (denominator-power-of-ten "1") (value "3.3")))))
	(d-let
		(p-assign (ident "all_int_list"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign (ident "all_str_list"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign (ident "all_float_list"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign (ident "get_empty"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_list)))
	(d-let
		(p-assign (ident "empty_int_list"))
		(e-call
			(e-lookup-local
				(p-assign (ident "get_empty")))
			(e-num (value "42"))))
	(d-let
		(p-assign (ident "empty_str_list"))
		(e-call
			(e-lookup-local
				(p-assign (ident "get_empty")))
			(e-string
				(e-literal (string "test")))))
	(d-let
		(p-assign (ident "main"))
		(e-closure
			(captures
				(capture (ident "all_int_list"))
				(capture (ident "all_str_list"))
				(capture (ident "all_float_list")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "len1"))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local
								(p-assign (ident "all_int_list")))))
					(s-let
						(p-assign (ident "len2"))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local
								(p-assign (ident "all_str_list")))))
					(s-let
						(p-assign (ident "len3"))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local
								(p-assign (ident "all_float_list")))))
					(e-binop (op "add")
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "len1")))
							(e-lookup-local
								(p-assign (ident "len2"))))
						(e-lookup-local
							(p-assign (ident "len3")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(_elem)"))
		(patt (type "List(Num(_size))"))
		(patt (type "List(Error)"))
		(patt (type "List(Num(Frac(_size)))"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "_arg -> List(_elem)"))
		(patt (type "List(_elem)"))
		(patt (type "Error"))
		(patt (type "_arg -> Error")))
	(expressions
		(expr (type "List(_elem)"))
		(expr (type "List(Num(_size))"))
		(expr (type "List(Error)"))
		(expr (type "List(Num(Frac(_size)))"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "_arg -> List(_elem)"))
		(expr (type "List(_elem)"))
		(expr (type "Error"))
		(expr (type "_arg -> Error"))))
~~~
