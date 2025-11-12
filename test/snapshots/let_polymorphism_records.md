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
# TODO: 11/06/2025 currently record ext syntax is not parsable
# update_data : { ..a, data: b }, b -> { ..a, data: b }
update_data = |container, new_value| { ..container, data: new_value }

# Used with different record types
updated_int = update_data(int_container, 100)
updated_str = update_data(str_container, "world")
updated_mismatch = update_data(str_container, 99)

# Function returning polymorphic record
identity_record = |x| { value: x }

# Used at different types
int_record = identity_record(42)
str_record = identity_record("test")
list_record = identity_record([1, 2, 3])

main = |_| {
	# Mismatch to snapshot infererd type of update_data
	1 + update_data

	# Access polymorphic fields
	int_container.count + str_container.count
}
~~~
# EXPECTED
TYPE DOES NOT HAVE METHODS - let_polymorphism_records.md:26:47:26:49
TYPE MISMATCH - let_polymorphism_records.md:38:6:38:17
# PROBLEMS
**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_int_digits` on a type that doesn't support methods:
**let_polymorphism_records.md:26:47:26:49:**
```roc
updated_mismatch = update_data(str_container, 99)
```
                                              ^^

This type doesn't support methods:
    _Str_



**TYPE MISMATCH**
This expression is used in an unexpected way:
**let_polymorphism_records.md:38:6:38:17:**
```roc
	1 + update_data
```
	    ^^^^^^^^^^^

It has the type:
    _{ ..a, data: b }, b -> { ..a, data: b }_

But I expected it to be:
    _Num(_size)_

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
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,Int,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
Int,OpPlus,LowerIdent,
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
				(e-record
					(ext
						(e-ident (raw "container")))
					(field (field "data")
						(e-ident (raw "new_value"))))))
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
			(p-ident (raw "updated_mismatch"))
			(e-apply
				(e-ident (raw "update_data"))
				(e-ident (raw "str_container"))
				(e-int (raw "99"))))
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
							(e-int (raw "1"))
							(e-ident (raw "update_data")))
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
NO CHANGE
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
			(e-record
				(ext
					(e-lookup-local
						(p-assign (ident "container"))))
				(fields
					(field (name "data")
						(e-lookup-local
							(p-assign (ident "new_value"))))))))
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
		(p-assign (ident "updated_mismatch"))
		(e-call
			(e-lookup-local
				(p-assign (ident "update_data")))
			(e-lookup-local
				(p-assign (ident "str_container")))
			(e-num (value "99"))))
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
				(capture (ident "update_data"))
				(capture (ident "int_container"))
				(capture (ident "str_container")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-expr
						(e-binop (op "add")
							(e-num (value "1"))
							(e-lookup-local
								(p-assign (ident "update_data")))))
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
		(patt (type "_a where [_b.from_int_digits : _arg -> _ret]"))
		(patt (type "Num(Frac(_size))"))
		(patt (type "Str"))
		(patt (type "List(_elem)"))
		(patt (type "List(Num(Frac(_size)))"))
		(patt (type "a -> { count: _field, data: a } where [_b.from_int_digits : _arg -> _ret]"))
		(patt (type "{ count: _field, data: _field2 } where [_a.from_int_digits : _arg -> _ret, _b.from_int_digits : _arg2 -> _ret2]"))
		(patt (type "{ count: _field, data: Str } where [_a.from_int_digits : _arg -> _ret]"))
		(patt (type "{ count: _field, data: List(_elem) } where [_a.from_int_digits : _arg -> _ret]"))
		(patt (type "{ ..a, data: b }, b -> { ..a, data: b }"))
		(patt (type "{ count: _field, data: _field2 } where [_a.from_int_digits : _arg -> _ret, _b.from_int_digits : _arg2 -> _ret2]"))
		(patt (type "{ count: _field, data: Str } where [_a.from_int_digits : _arg -> _ret]"))
		(patt (type "{ count: _field, data: Str } where [_a.from_int_digits : _arg -> _ret]"))
		(patt (type "a -> { value: a }"))
		(patt (type "{ value: _field } where [_a.from_int_digits : _arg -> _ret]"))
		(patt (type "{ value: Str }"))
		(patt (type "{ value: List(_elem) } where [_a.from_int_digits : _arg -> _ret]"))
		(patt (type "_arg -> Num(_size)")))
	(expressions
		(expr (type "_a where [_b.from_int_digits : _arg -> _ret]"))
		(expr (type "Num(Frac(_size))"))
		(expr (type "Str"))
		(expr (type "List(_elem)"))
		(expr (type "List(Num(Frac(_size)))"))
		(expr (type "a -> { count: _field, data: a } where [_b.from_int_digits : _arg -> _ret]"))
		(expr (type "{ count: _field, data: _field2 } where [_a.from_int_digits : _arg -> _ret, _b.from_int_digits : _arg2 -> _ret2]"))
		(expr (type "{ count: _field, data: Str } where [_a.from_int_digits : _arg -> _ret]"))
		(expr (type "{ count: _field, data: List(_elem) } where [_a.from_int_digits : _arg -> _ret]"))
		(expr (type "{ ..a, data: b }, b -> { ..a, data: b }"))
		(expr (type "{ count: _field, data: _field2 } where [_a.from_int_digits : _arg -> _ret, _b.from_int_digits : _arg2 -> _ret2]"))
		(expr (type "{ count: _field, data: Str } where [_a.from_int_digits : _arg -> _ret]"))
		(expr (type "{ count: _field, data: Str } where [_a.from_int_digits : _arg -> _ret]"))
		(expr (type "a -> { value: a }"))
		(expr (type "{ value: _field } where [_a.from_int_digits : _arg -> _ret]"))
		(expr (type "{ value: Str }"))
		(expr (type "{ value: List(_elem) } where [_a.from_int_digits : _arg -> _ret]"))
		(expr (type "_arg -> Num(_size)"))))
~~~
