# META
~~~ini
description=Let-polymorphism with records
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic values for polymorphism testing
num = 42
frac = 4.2
str = "hello"
my_empty_list = []
my_nonempty_list = [num, frac]

# Record with polymorphic field
make_container = |value| { data: value, count: 1 }

# Used with different types
int_container = make_container(num)
str_container = make_container(str)
list_container = make_container(my_empty_list)

# Polymorphic record update
update_data = |container, new_value| { container & data: new_value }

# Used with different record types
updated_int = update_data(int_container, 100)
updated_str = update_data(str_container, "world")

# Function returning polymorphic record
identity_record = |x| { value: x }

# Used at different types
int_record = identity_record(42)
str_record = identity_record("test")
list_record = identity_record([1, 2, 3])

main = |_| {
    # Access polymorphic fields
    int_container.count + str_container.count
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - let_polymorphism_records.md:19:50:19:51
UNRECOGNIZED SYNTAX - let_polymorphism_records.md:19:50:19:51
UNUSED VARIABLE - let_polymorphism_records.md:19:27:19:36
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **&** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**let_polymorphism_records.md:19:50:19:51:**
```roc
update_data = |container, new_value| { container & data: new_value }
```
                                                 ^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**let_polymorphism_records.md:19:50:19:51:**
```roc
update_data = |container, new_value| { container & data: new_value }
```
                                                 ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNUSED VARIABLE**
Variable `new_value` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_new_value` to suppress this warning.
The unused variable is declared here:
**let_polymorphism_records.md:19:27:19:36:**
```roc
update_data = |container, new_value| { container & data: new_value }
```
                          ^^^^^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Float,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,OpenSquare,CloseSquare,
LowerIdent,OpAssign,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,LowerIdent,OpAmpersand,LowerIdent,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,NoSpaceDotLowerIdent,OpPlus,LowerIdent,NoSpaceDotLowerIdent,
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
			(p-ident (raw "num"))
			(e-int (raw "42")))
		(s-decl
			(p-ident (raw "frac"))
			(e-frac (raw "4.2")))
		(s-decl
			(p-ident (raw "str"))
			(e-string
				(e-string-part (raw "hello"))))
		(s-decl
			(p-ident (raw "my_empty_list"))
			(e-list))
		(s-decl
			(p-ident (raw "my_nonempty_list"))
			(e-list
				(e-ident (raw "num"))
				(e-ident (raw "frac"))))
		(s-decl
			(p-ident (raw "make_container"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-record
					(field (field "data")
						(e-ident (raw "value")))
					(field (field "count")
						(e-int (raw "1"))))))
		(s-decl
			(p-ident (raw "int_container"))
			(e-apply
				(e-ident (raw "make_container"))
				(e-ident (raw "num"))))
		(s-decl
			(p-ident (raw "str_container"))
			(e-apply
				(e-ident (raw "make_container"))
				(e-ident (raw "str"))))
		(s-decl
			(p-ident (raw "list_container"))
			(e-apply
				(e-ident (raw "make_container"))
				(e-ident (raw "my_empty_list"))))
		(s-decl
			(p-ident (raw "update_data"))
			(e-lambda
				(args
					(p-ident (raw "container"))
					(p-ident (raw "new_value")))
				(e-block
					(statements
						(e-ident (raw "container"))
						(e-malformed (reason "expr_unexpected_token"))
						(s-type-anno (name "data")
							(ty-var (raw "new_value")))))))
		(s-decl
			(p-ident (raw "updated_int"))
			(e-apply
				(e-ident (raw "update_data"))
				(e-ident (raw "int_container"))
				(e-int (raw "100"))))
		(s-decl
			(p-ident (raw "updated_str"))
			(e-apply
				(e-ident (raw "update_data"))
				(e-ident (raw "str_container"))
				(e-string
					(e-string-part (raw "world")))))
		(s-decl
			(p-ident (raw "identity_record"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-record
					(field (field "value")
						(e-ident (raw "x"))))))
		(s-decl
			(p-ident (raw "int_record"))
			(e-apply
				(e-ident (raw "identity_record"))
				(e-int (raw "42"))))
		(s-decl
			(p-ident (raw "str_record"))
			(e-apply
				(e-ident (raw "identity_record"))
				(e-string
					(e-string-part (raw "test")))))
		(s-decl
			(p-ident (raw "list_record"))
			(e-apply
				(e-ident (raw "identity_record"))
				(e-list
					(e-int (raw "1"))
					(e-int (raw "2"))
					(e-int (raw "3")))))
		(s-decl
			(p-ident (raw "main"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(e-binop (op "+")
							(e-field-access
								(e-ident (raw "int_container"))
								(e-ident (raw "count")))
							(e-field-access
								(e-ident (raw "str_container"))
								(e-ident (raw "count"))))))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic values for polymorphism testing
num = 42
frac = 4.2
str = "hello"
my_empty_list = []
my_nonempty_list = [num, frac]

# Record with polymorphic field
make_container = |value| { data: value, count: 1 }

# Used with different types
int_container = make_container(num)
str_container = make_container(str)
list_container = make_container(my_empty_list)

# Polymorphic record update
update_data = |container, new_value| {
	container
		data : new_value
}

# Used with different record types
updated_int = update_data(int_container, 100)
updated_str = update_data(str_container, "world")

# Function returning polymorphic record
identity_record = |x| { value: x }

# Used at different types
int_record = identity_record(42)
str_record = identity_record("test")
list_record = identity_record([1, 2, 3])

main = |_| {
	# Access polymorphic fields
	int_container.count + str_container.count
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "num"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "frac"))
		(e-dec-small (numerator "42") (denominator-power-of-ten "1") (value "4.2")))
	(d-let
		(p-assign (ident "str"))
		(e-string
			(e-literal (string "hello"))))
	(d-let
		(p-assign (ident "my_empty_list"))
		(e-empty_list))
	(d-let
		(p-assign (ident "my_nonempty_list"))
		(e-list
			(elems
				(e-lookup-local
					(p-assign (ident "num")))
				(e-lookup-local
					(p-assign (ident "frac"))))))
	(d-let
		(p-assign (ident "make_container"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-record
				(fields
					(field (name "data")
						(e-lookup-local
							(p-assign (ident "value"))))
					(field (name "count")
						(e-num (value "1")))))))
	(d-let
		(p-assign (ident "int_container"))
		(e-call
			(e-lookup-local
				(p-assign (ident "make_container")))
			(e-lookup-local
				(p-assign (ident "num")))))
	(d-let
		(p-assign (ident "str_container"))
		(e-call
			(e-lookup-local
				(p-assign (ident "make_container")))
			(e-lookup-local
				(p-assign (ident "str")))))
	(d-let
		(p-assign (ident "list_container"))
		(e-call
			(e-lookup-local
				(p-assign (ident "make_container")))
			(e-lookup-local
				(p-assign (ident "my_empty_list")))))
	(d-let
		(p-assign (ident "update_data"))
		(e-lambda
			(args
				(p-assign (ident "container"))
				(p-assign (ident "new_value")))
			(e-block
				(s-expr
					(e-lookup-local
						(p-assign (ident "container"))))
				(s-expr
					(e-runtime-error (tag "expr_not_canonicalized")))
				(e-empty_record))))
	(d-let
		(p-assign (ident "updated_int"))
		(e-call
			(e-lookup-local
				(p-assign (ident "update_data")))
			(e-lookup-local
				(p-assign (ident "int_container")))
			(e-num (value "100"))))
	(d-let
		(p-assign (ident "updated_str"))
		(e-call
			(e-lookup-local
				(p-assign (ident "update_data")))
			(e-lookup-local
				(p-assign (ident "str_container")))
			(e-string
				(e-literal (string "world")))))
	(d-let
		(p-assign (ident "identity_record"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-record
				(fields
					(field (name "value")
						(e-lookup-local
							(p-assign (ident "x"))))))))
	(d-let
		(p-assign (ident "int_record"))
		(e-call
			(e-lookup-local
				(p-assign (ident "identity_record")))
			(e-num (value "42"))))
	(d-let
		(p-assign (ident "str_record"))
		(e-call
			(e-lookup-local
				(p-assign (ident "identity_record")))
			(e-string
				(e-literal (string "test")))))
	(d-let
		(p-assign (ident "list_record"))
		(e-call
			(e-lookup-local
				(p-assign (ident "identity_record")))
			(e-list
				(elems
					(e-num (value "1"))
					(e-num (value "2"))
					(e-num (value "3"))))))
	(d-let
		(p-assign (ident "main"))
		(e-closure
			(captures
				(capture (ident "str_container"))
				(capture (ident "int_container")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(e-binop (op "add")
						(e-dot-access (field "count")
							(receiver
								(e-lookup-local
									(p-assign (ident "int_container")))))
						(e-dot-access (field "count")
							(receiver
								(e-lookup-local
									(p-assign (ident "str_container")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Frac(_size))"))
		(patt (type "Num(Frac(_size))"))
		(patt (type "Str"))
		(patt (type "List(_elem)"))
		(patt (type "List(Num(Frac(_size)))"))
		(patt (type "a -> { count: Num(_size), data: a }"))
		(patt (type "{ count: Num(_size), data: Num(Frac(_size2)) }"))
		(patt (type "{ count: Num(_size), data: Str }"))
		(patt (type "{ count: Num(_size), data: List(_elem) }"))
		(patt (type "_arg, _arg2 -> {}"))
		(patt (type "{}"))
		(patt (type "{}"))
		(patt (type "a -> { value: a }"))
		(patt (type "{ value: Num(_size) }"))
		(patt (type "{ value: Str }"))
		(patt (type "{ value: List(Num(_size)) }"))
		(patt (type "_arg -> Num(_size)")))
	(expressions
		(expr (type "Num(Frac(_size))"))
		(expr (type "Num(Frac(_size))"))
		(expr (type "Str"))
		(expr (type "List(_elem)"))
		(expr (type "List(Num(Frac(_size)))"))
		(expr (type "a -> { count: Num(_size), data: a }"))
		(expr (type "{ count: Num(_size), data: Num(Frac(_size2)) }"))
		(expr (type "{ count: Num(_size), data: Str }"))
		(expr (type "{ count: Num(_size), data: List(_elem) }"))
		(expr (type "_arg, _arg2 -> {}"))
		(expr (type "{}"))
		(expr (type "{}"))
		(expr (type "a -> { value: a }"))
		(expr (type "{ value: Num(_size) }"))
		(expr (type "{ value: Str }"))
		(expr (type "{ value: List(Num(_size)) }"))
		(expr (type "_arg -> Num(_size)"))))
~~~
