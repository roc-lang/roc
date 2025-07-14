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
UNUSED VARIABLE - let_polymorphism_records.md:19:27:19:36
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **&** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**let_polymorphism_records.md:19:50:19:51:**
```roc
update_data = |container, new_value| { container & data: new_value }
```
                                                 ^


**UNUSED VARIABLE**
Variable ``new_value`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_new_value` to suppress this warning.
The unused variable is declared here:
**let_polymorphism_records.md:19:27:19:36:**
```roc
update_data = |container, new_value| { container & data: new_value }
```
                          ^^^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),Int(4:7-4:9),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),Float(5:8-5:11),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),StringStart(6:7-6:8),StringPart(6:8-6:13),StringEnd(6:13-6:14),
LowerIdent(7:1-7:14),OpAssign(7:15-7:16),OpenSquare(7:17-7:18),CloseSquare(7:18-7:19),
LowerIdent(8:1-8:17),OpAssign(8:18-8:19),OpenSquare(8:20-8:21),LowerIdent(8:21-8:24),Comma(8:24-8:25),LowerIdent(8:26-8:30),CloseSquare(8:30-8:31),
LowerIdent(11:1-11:15),OpAssign(11:16-11:17),OpBar(11:18-11:19),LowerIdent(11:19-11:24),OpBar(11:24-11:25),OpenCurly(11:26-11:27),LowerIdent(11:28-11:32),OpColon(11:32-11:33),LowerIdent(11:34-11:39),Comma(11:39-11:40),LowerIdent(11:41-11:46),OpColon(11:46-11:47),Int(11:48-11:49),CloseCurly(11:50-11:51),
LowerIdent(14:1-14:14),OpAssign(14:15-14:16),LowerIdent(14:17-14:31),NoSpaceOpenRound(14:31-14:32),LowerIdent(14:32-14:35),CloseRound(14:35-14:36),
LowerIdent(15:1-15:14),OpAssign(15:15-15:16),LowerIdent(15:17-15:31),NoSpaceOpenRound(15:31-15:32),LowerIdent(15:32-15:35),CloseRound(15:35-15:36),
LowerIdent(16:1-16:15),OpAssign(16:16-16:17),LowerIdent(16:18-16:32),NoSpaceOpenRound(16:32-16:33),LowerIdent(16:33-16:46),CloseRound(16:46-16:47),
LowerIdent(19:1-19:12),OpAssign(19:13-19:14),OpBar(19:15-19:16),LowerIdent(19:16-19:25),Comma(19:25-19:26),LowerIdent(19:27-19:36),OpBar(19:36-19:37),OpenCurly(19:38-19:39),LowerIdent(19:40-19:49),OpAmpersand(19:50-19:51),LowerIdent(19:52-19:56),OpColon(19:56-19:57),LowerIdent(19:58-19:67),CloseCurly(19:68-19:69),
LowerIdent(22:1-22:12),OpAssign(22:13-22:14),LowerIdent(22:15-22:26),NoSpaceOpenRound(22:26-22:27),LowerIdent(22:27-22:40),Comma(22:40-22:41),Int(22:42-22:45),CloseRound(22:45-22:46),
LowerIdent(23:1-23:12),OpAssign(23:13-23:14),LowerIdent(23:15-23:26),NoSpaceOpenRound(23:26-23:27),LowerIdent(23:27-23:40),Comma(23:40-23:41),StringStart(23:42-23:43),StringPart(23:43-23:48),StringEnd(23:48-23:49),CloseRound(23:49-23:50),
LowerIdent(26:1-26:16),OpAssign(26:17-26:18),OpBar(26:19-26:20),LowerIdent(26:20-26:21),OpBar(26:21-26:22),OpenCurly(26:23-26:24),LowerIdent(26:25-26:30),OpColon(26:30-26:31),LowerIdent(26:32-26:33),CloseCurly(26:34-26:35),
LowerIdent(29:1-29:11),OpAssign(29:12-29:13),LowerIdent(29:14-29:29),NoSpaceOpenRound(29:29-29:30),Int(29:30-29:32),CloseRound(29:32-29:33),
LowerIdent(30:1-30:11),OpAssign(30:12-30:13),LowerIdent(30:14-30:29),NoSpaceOpenRound(30:29-30:30),StringStart(30:30-30:31),StringPart(30:31-30:35),StringEnd(30:35-30:36),CloseRound(30:36-30:37),
LowerIdent(31:1-31:12),OpAssign(31:13-31:14),LowerIdent(31:15-31:30),NoSpaceOpenRound(31:30-31:31),OpenSquare(31:31-31:32),Int(31:32-31:33),Comma(31:33-31:34),Int(31:35-31:36),Comma(31:36-31:37),Int(31:38-31:39),CloseSquare(31:39-31:40),CloseRound(31:40-31:41),
LowerIdent(33:1-33:5),OpAssign(33:6-33:7),OpBar(33:8-33:9),Underscore(33:9-33:10),OpBar(33:10-33:11),OpenCurly(33:12-33:13),
LowerIdent(35:5-35:18),NoSpaceDotLowerIdent(35:18-35:24),OpPlus(35:25-35:26),LowerIdent(35:27-35:40),NoSpaceDotLowerIdent(35:40-35:46),
CloseCurly(36:1-36:2),EndOfFile(36:2-36:2),
~~~
# PARSE
~~~clojure
(file @1.1-36.2
	(app @1.1-1.56
		(provides @1.5-1.11
			(exposed-lower-ident @1.6-1.10
				(text "main")))
		(record-field @1.14-1.54 (name "pf")
			(e-string @1.27-1.54
				(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))
		(packages @1.12-1.56
			(record-field @1.14-1.54 (name "pf")
				(e-string @1.27-1.54
					(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-decl @4.1-4.9
			(p-ident @4.1-4.4 (raw "num"))
			(e-int @4.7-4.9 (raw "42")))
		(s-decl @5.1-5.11
			(p-ident @5.1-5.5 (raw "frac"))
			(e-frac @5.8-5.11 (raw "4.2")))
		(s-decl @6.1-6.14
			(p-ident @6.1-6.4 (raw "str"))
			(e-string @6.7-6.14
				(e-string-part @6.8-6.13 (raw "hello"))))
		(s-decl @7.1-7.19
			(p-ident @7.1-7.14 (raw "my_empty_list"))
			(e-list @7.17-7.19))
		(s-decl @8.1-8.31
			(p-ident @8.1-8.17 (raw "my_nonempty_list"))
			(e-list @8.20-8.31
				(e-ident @8.21-8.24 (raw "num"))
				(e-ident @8.26-8.30 (raw "frac"))))
		(s-decl @11.1-11.51
			(p-ident @11.1-11.15 (raw "make_container"))
			(e-lambda @11.18-11.51
				(args
					(p-ident @11.19-11.24 (raw "value")))
				(e-record @11.26-11.51
					(field (field "data")
						(e-ident @11.34-11.39 (raw "value")))
					(field (field "count")
						(e-int @11.48-11.49 (raw "1"))))))
		(s-decl @14.1-14.36
			(p-ident @14.1-14.14 (raw "int_container"))
			(e-apply @14.17-14.36
				(e-ident @14.17-14.31 (raw "make_container"))
				(e-ident @14.32-14.35 (raw "num"))))
		(s-decl @15.1-15.36
			(p-ident @15.1-15.14 (raw "str_container"))
			(e-apply @15.17-15.36
				(e-ident @15.17-15.31 (raw "make_container"))
				(e-ident @15.32-15.35 (raw "str"))))
		(s-decl @16.1-16.47
			(p-ident @16.1-16.15 (raw "list_container"))
			(e-apply @16.18-16.47
				(e-ident @16.18-16.32 (raw "make_container"))
				(e-ident @16.33-16.46 (raw "my_empty_list"))))
		(s-decl @19.1-19.69
			(p-ident @19.1-19.12 (raw "update_data"))
			(e-lambda @19.15-19.69
				(args
					(p-ident @19.16-19.25 (raw "container"))
					(p-ident @19.27-19.36 (raw "new_value")))
				(e-block @19.38-19.69
					(statements
						(e-ident @19.40-19.49 (raw "container"))
						(e-malformed @19.50-19.51 (reason "expr_unexpected_token"))
						(s-type-anno @19.52-19.67 (name "data")
							(ty-var @19.58-19.67 (raw "new_value")))))))
		(s-decl @22.1-22.46
			(p-ident @22.1-22.12 (raw "updated_int"))
			(e-apply @22.15-22.46
				(e-ident @22.15-22.26 (raw "update_data"))
				(e-ident @22.27-22.40 (raw "int_container"))
				(e-int @22.42-22.45 (raw "100"))))
		(s-decl @23.1-23.50
			(p-ident @23.1-23.12 (raw "updated_str"))
			(e-apply @23.15-23.50
				(e-ident @23.15-23.26 (raw "update_data"))
				(e-ident @23.27-23.40 (raw "str_container"))
				(e-string @23.42-23.49
					(e-string-part @23.43-23.48 (raw "world")))))
		(s-decl @26.1-26.35
			(p-ident @26.1-26.16 (raw "identity_record"))
			(e-lambda @26.19-26.35
				(args
					(p-ident @26.20-26.21 (raw "x")))
				(e-record @26.23-26.35
					(field (field "value")
						(e-ident @26.32-26.33 (raw "x"))))))
		(s-decl @29.1-29.33
			(p-ident @29.1-29.11 (raw "int_record"))
			(e-apply @29.14-29.33
				(e-ident @29.14-29.29 (raw "identity_record"))
				(e-int @29.30-29.32 (raw "42"))))
		(s-decl @30.1-30.37
			(p-ident @30.1-30.11 (raw "str_record"))
			(e-apply @30.14-30.37
				(e-ident @30.14-30.29 (raw "identity_record"))
				(e-string @30.30-30.36
					(e-string-part @30.31-30.35 (raw "test")))))
		(s-decl @31.1-31.41
			(p-ident @31.1-31.12 (raw "list_record"))
			(e-apply @31.15-31.41
				(e-ident @31.15-31.30 (raw "identity_record"))
				(e-list @31.31-31.40
					(e-int @31.32-31.33 (raw "1"))
					(e-int @31.35-31.36 (raw "2"))
					(e-int @31.38-31.39 (raw "3")))))
		(s-decl @33.1-36.2
			(p-ident @33.1-33.5 (raw "main"))
			(e-lambda @33.8-36.2
				(args
					(p-underscore))
				(e-block @33.12-36.2
					(statements
						(e-binop @35.5-35.46 (op "+")
							(e-field-access @35.5-35.24
								(e-ident @35.5-35.18 (raw "int_container"))
								(e-ident @35.18-35.24 (raw "count")))
							(e-field-access @35.27-35.46
								(e-ident @35.27-35.40 (raw "str_container"))
								(e-ident @35.40-35.46 (raw "count"))))))))))
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
make_container = |value| {data: value, count: 1}

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
identity_record = |x| {value: x}

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
		(p-assign @4.1-4.4 (ident "num"))
		(e-int @4.7-4.9 (value "42")))
	(d-let
		(p-assign @5.1-5.5 (ident "frac"))
		(e-dec-small @5.8-5.11 (numerator "42") (denominator-power-of-ten "1") (value "4.2")))
	(d-let
		(p-assign @6.1-6.4 (ident "str"))
		(e-string @6.7-6.14
			(e-literal @6.8-6.13 (string "hello"))))
	(d-let
		(p-assign @7.1-7.14 (ident "my_empty_list"))
		(e-empty_list @7.17-7.19))
	(d-let
		(p-assign @8.1-8.17 (ident "my_nonempty_list"))
		(e-list @8.20-8.31
			(elems
				(e-lookup-local @8.21-8.24
					(p-assign @4.1-4.4 (ident "num")))
				(e-lookup-local @8.26-8.30
					(p-assign @5.1-5.5 (ident "frac"))))))
	(d-let
		(p-assign @11.1-11.15 (ident "make_container"))
		(e-lambda @11.18-11.51
			(args
				(p-assign @11.19-11.24 (ident "value")))
			(e-record @11.26-11.51
				(fields
					(field (name "data")
						(e-lookup-local @11.34-11.39
							(p-assign @11.19-11.24 (ident "value"))))
					(field (name "count")
						(e-int @11.48-11.49 (value "1")))))))
	(d-let
		(p-assign @14.1-14.14 (ident "int_container"))
		(e-call @14.17-14.36
			(e-lookup-local @14.17-14.31
				(p-assign @11.1-11.15 (ident "make_container")))
			(e-lookup-local @14.32-14.35
				(p-assign @4.1-4.4 (ident "num")))))
	(d-let
		(p-assign @15.1-15.14 (ident "str_container"))
		(e-call @15.17-15.36
			(e-lookup-local @15.17-15.31
				(p-assign @11.1-11.15 (ident "make_container")))
			(e-lookup-local @15.32-15.35
				(p-assign @6.1-6.4 (ident "str")))))
	(d-let
		(p-assign @16.1-16.15 (ident "list_container"))
		(e-call @16.18-16.47
			(e-lookup-local @16.18-16.32
				(p-assign @11.1-11.15 (ident "make_container")))
			(e-lookup-local @16.33-16.46
				(p-assign @7.1-7.14 (ident "my_empty_list")))))
	(d-let
		(p-assign @19.1-19.12 (ident "update_data"))
		(e-lambda @19.15-19.69
			(args
				(p-assign @19.16-19.25 (ident "container"))
				(p-assign @19.27-19.36 (ident "new_value")))
			(e-block @19.38-19.69
				(s-expr @19.40-19.49
					(e-lookup-local @19.40-19.49
						(p-assign @19.16-19.25 (ident "container"))))
				(s-type-anno @19.52-19.67 (name "data")
					(ty-var @19.58-19.67 (name "new_value")))
				(e-tuple @19.52-19.67
					(elems)))))
	(d-let
		(p-assign @22.1-22.12 (ident "updated_int"))
		(e-call @22.15-22.46
			(e-lookup-local @22.15-22.26
				(p-assign @19.1-19.12 (ident "update_data")))
			(e-lookup-local @22.27-22.40
				(p-assign @14.1-14.14 (ident "int_container")))
			(e-int @22.42-22.45 (value "100"))))
	(d-let
		(p-assign @23.1-23.12 (ident "updated_str"))
		(e-call @23.15-23.50
			(e-lookup-local @23.15-23.26
				(p-assign @19.1-19.12 (ident "update_data")))
			(e-lookup-local @23.27-23.40
				(p-assign @15.1-15.14 (ident "str_container")))
			(e-string @23.42-23.49
				(e-literal @23.43-23.48 (string "world")))))
	(d-let
		(p-assign @26.1-26.16 (ident "identity_record"))
		(e-lambda @26.19-26.35
			(args
				(p-assign @26.20-26.21 (ident "x")))
			(e-record @26.23-26.35
				(fields
					(field (name "value")
						(e-lookup-local @26.32-26.33
							(p-assign @26.20-26.21 (ident "x"))))))))
	(d-let
		(p-assign @29.1-29.11 (ident "int_record"))
		(e-call @29.14-29.33
			(e-lookup-local @29.14-29.29
				(p-assign @26.1-26.16 (ident "identity_record")))
			(e-int @29.30-29.32 (value "42"))))
	(d-let
		(p-assign @30.1-30.11 (ident "str_record"))
		(e-call @30.14-30.37
			(e-lookup-local @30.14-30.29
				(p-assign @26.1-26.16 (ident "identity_record")))
			(e-string @30.30-30.36
				(e-literal @30.31-30.35 (string "test")))))
	(d-let
		(p-assign @31.1-31.12 (ident "list_record"))
		(e-call @31.15-31.41
			(e-lookup-local @31.15-31.30
				(p-assign @26.1-26.16 (ident "identity_record")))
			(e-list @31.31-31.40
				(elems
					(e-int @31.32-31.33 (value "1"))
					(e-int @31.35-31.36 (value "2"))
					(e-int @31.38-31.39 (value "3"))))))
	(d-let
		(p-assign @33.1-33.5 (ident "main"))
		(e-lambda @33.8-36.2
			(args
				(p-underscore @33.9-33.10))
			(e-block @33.12-36.2
				(e-binop @35.5-35.46 (op "add")
					(e-dot-access @35.5-35.24 (field "count")
						(receiver
							(e-lookup-local @35.5-35.18
								(p-assign @14.1-14.14 (ident "int_container")))))
					(e-dot-access @35.27-35.46 (field "count")
						(receiver
							(e-lookup-local @35.27-35.40
								(p-assign @15.1-15.14 (ident "str_container"))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Frac(_size)"))
		(patt @5.1-5.5 (type "Frac(_size)"))
		(patt @6.1-6.4 (type "Str"))
		(patt @7.1-7.14 (type "List(_elem)"))
		(patt @8.1-8.17 (type "List(Frac(_size))"))
		(patt @11.1-11.15 (type "_arg -> { data: _field, count: Num(_size) }"))
		(patt @14.1-14.14 (type "{ data: _field, count: Num(_size) }"))
		(patt @15.1-15.14 (type "{ data: _field, count: Num(_size) }"))
		(patt @16.1-16.15 (type "{ data: _field, count: Num(_size) }"))
		(patt @19.1-19.12 (type "_arg, _arg2 -> _ret"))
		(patt @22.1-22.12 (type "_a"))
		(patt @23.1-23.12 (type "_a"))
		(patt @26.1-26.16 (type "_arg -> { value: _field }"))
		(patt @29.1-29.11 (type "{ value: _field }"))
		(patt @30.1-30.11 (type "{ value: _field }"))
		(patt @31.1-31.12 (type "{ value: _field }"))
		(patt @33.1-33.5 (type "_arg -> _ret")))
	(expressions
		(expr @4.7-4.9 (type "Frac(_size)"))
		(expr @5.8-5.11 (type "Frac(_size)"))
		(expr @6.7-6.14 (type "Str"))
		(expr @7.17-7.19 (type "List(_elem)"))
		(expr @8.20-8.31 (type "List(Frac(_size))"))
		(expr @11.18-11.51 (type "_arg -> { data: _field, count: Num(_size) }"))
		(expr @14.17-14.36 (type "{ data: _field, count: Num(_size) }"))
		(expr @15.17-15.36 (type "{ data: _field, count: Num(_size) }"))
		(expr @16.18-16.47 (type "{ data: _field, count: Num(_size) }"))
		(expr @19.15-19.69 (type "_arg, _arg2 -> _ret"))
		(expr @22.15-22.46 (type "_a"))
		(expr @23.15-23.50 (type "_a"))
		(expr @26.19-26.35 (type "_arg -> { value: _field }"))
		(expr @29.14-29.33 (type "{ value: _field }"))
		(expr @30.14-30.37 (type "{ value: _field }"))
		(expr @31.15-31.41 (type "{ value: _field }"))
		(expr @33.8-36.2 (type "_arg -> _ret"))))
~~~
