# META
~~~ini
description=Type aliases and declarations with various forms
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Simple type alias
UserId : U64

# Generic type alias
Result(ok, err) : [Ok(ok), Err(err)]

# Record type alias
Person : { name : Str, age : U64 }

# Function type alias
MapFn(a, b) : a -> b

# Complex nested type alias
ApiResponse(data) : Result(data, Str)

# Type declaration with tag union
Color : [Red, Green, Blue, Custom(U8, U8, U8)]

# Type declaration with records and generics
Container(item) : {
    contents : List(item),
    metadata : { size : U64, created : Str }
}

main! = |_| {
    # Use the types to validate they work
    userId : UserId
    userId = 123

    person : Person
    person = { name: "Alice", age: 30 }

    color : Color
    color = Red

    userId
}
~~~
# EXPECTED
TYPE REDECLARED - type_alias_decl.md:7:1:7:37
UNUSED VARIABLE - type_alias_decl.md:33:5:33:11
UNUSED VARIABLE - type_alias_decl.md:36:5:36:10
# PROBLEMS
**TYPE REDECLARED**
The type _Result_ is being redeclared.

The redeclaration is here:
**type_alias_decl.md:7:1:7:37:**
```roc
Result(ok, err) : [Ok(ok), Err(err)]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

But _Result_ was already declared here:
**type_alias_decl.md:1:1:1:1:**
```roc
app [main!] { pf: platform "../basic-cli/main.roc" }
```
^


**UNUSED VARIABLE**
Variable `person` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_person` to suppress this warning.
The unused variable is declared here:
**type_alias_decl.md:33:5:33:11:**
```roc
    person = { name: "Alice", age: 30 }
```
    ^^^^^^


**UNUSED VARIABLE**
Variable `color` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_color` to suppress this warning.
The unused variable is declared here:
**type_alias_decl.md:36:5:36:10:**
```roc
    color = Red
```
    ^^^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
UpperIdent,OpColon,UpperIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,LowerIdent,OpArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,CloseRound,
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseRound,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-decl
			(header (name "UserId")
				(args))
			(ty (name "U64")))
		(s-type-decl
			(header (name "Result")
				(args
					(ty-var (raw "ok"))
					(ty-var (raw "err"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Ok"))
						(ty-var (raw "ok")))
					(ty-apply
						(ty (name "Err"))
						(ty-var (raw "err"))))))
		(s-type-decl
			(header (name "Person")
				(args))
			(ty-record
				(anno-record-field (name "name")
					(ty (name "Str")))
				(anno-record-field (name "age")
					(ty (name "U64")))))
		(s-type-decl
			(header (name "MapFn")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b"))))
		(s-type-decl
			(header (name "ApiResponse")
				(args
					(ty-var (raw "data"))))
			(ty-apply
				(ty (name "Result"))
				(ty-var (raw "data"))
				(ty (name "Str"))))
		(s-type-decl
			(header (name "Color")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Red"))
					(ty (name "Green"))
					(ty (name "Blue"))
					(ty-apply
						(ty (name "Custom"))
						(ty (name "U8"))
						(ty (name "U8"))
						(ty (name "U8"))))))
		(s-type-decl
			(header (name "Container")
				(args
					(ty-var (raw "item"))))
			(ty-record
				(anno-record-field (name "contents")
					(ty-apply
						(ty (name "List"))
						(ty-var (raw "item"))))
				(anno-record-field (name "metadata")
					(ty-record
						(anno-record-field (name "size")
							(ty (name "U64")))
						(anno-record-field (name "created")
							(ty (name "Str")))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-type-anno (name "userId")
							(ty (name "UserId")))
						(s-decl
							(p-ident (raw "userId"))
							(e-int (raw "123")))
						(s-type-anno (name "person")
							(ty (name "Person")))
						(s-decl
							(p-ident (raw "person"))
							(e-record
								(field (field "name")
									(e-string
										(e-string-part (raw "Alice"))))
								(field (field "age")
									(e-int (raw "30")))))
						(s-type-anno (name "color")
							(ty (name "Color")))
						(s-decl
							(p-ident (raw "color"))
							(e-tag (raw "Red")))
						(e-ident (raw "userId"))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Simple type alias
UserId : U64

# Generic type alias
Result(ok, err) : [Ok(ok), Err(err)]

# Record type alias
Person : { name : Str, age : U64 }

# Function type alias
MapFn(a, b) : a -> b

# Complex nested type alias
ApiResponse(data) : Result(data, Str)

# Type declaration with tag union
Color : [Red, Green, Blue, Custom(U8, U8, U8)]

# Type declaration with records and generics
Container(item) : {
	contents : List(item),
	metadata : { size : U64, created : Str },
}

main! = |_| {
	# Use the types to validate they work
	userId : UserId
	userId = 123

	person : Person
	person = { name: "Alice", age: 30 }

	color : Color
	color = Red

	userId
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "userId"))
					(e-num (value "123")))
				(s-let
					(p-assign (ident "person"))
					(e-record
						(fields
							(field (name "name")
								(e-string
									(e-literal (string "Alice"))))
							(field (name "age")
								(e-num (value "30"))))))
				(s-let
					(p-assign (ident "color"))
					(e-tag (name "Red")))
				(e-lookup-local
					(p-assign (ident "userId"))))))
	(s-alias-decl
		(ty-header (name "UserId"))
		(ty-lookup (name "U64") (builtin)))
	(s-alias-decl
		(ty-header (name "Result")
			(ty-args
				(ty-rigid-var (name "ok"))
				(ty-rigid-var (name "err"))))
		(ty-tag-union
			(ty-tag-name (name "Ok")
				(ty-rigid-var-lookup (ty-rigid-var (name "ok"))))
			(ty-tag-name (name "Err")
				(ty-rigid-var-lookup (ty-rigid-var (name "err"))))))
	(s-alias-decl
		(ty-header (name "Person"))
		(ty-record
			(field (field "name")
				(ty-lookup (name "Str") (builtin)))
			(field (field "age")
				(ty-lookup (name "U64") (builtin)))))
	(s-alias-decl
		(ty-header (name "MapFn")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-fn (effectful false)
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var (name "b")))))
	(s-alias-decl
		(ty-header (name "ApiResponse")
			(ty-args
				(ty-rigid-var (name "data"))))
		(ty-apply (name "Result") (external (module-idx "3") (target-node-idx "3"))
			(ty-rigid-var-lookup (ty-rigid-var (name "data")))
			(ty-lookup (name "Str") (builtin))))
	(s-alias-decl
		(ty-header (name "Color"))
		(ty-tag-union
			(ty-tag-name (name "Red"))
			(ty-tag-name (name "Green"))
			(ty-tag-name (name "Blue"))
			(ty-tag-name (name "Custom")
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin)))))
	(s-alias-decl
		(ty-header (name "Container")
			(ty-args
				(ty-rigid-var (name "item"))))
		(ty-record
			(field (field "contents")
				(ty-apply (name "List") (builtin)
					(ty-rigid-var-lookup (ty-rigid-var (name "item")))))
			(field (field "metadata")
				(ty-record
					(field (field "size")
						(ty-lookup (name "U64") (builtin)))
					(field (field "created")
						(ty-lookup (name "Str") (builtin))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> UserId")))
	(type_decls
		(alias (type "UserId")
			(ty-header (name "UserId")))
		(alias (type "Result(ok, err)")
			(ty-header (name "Result")
				(ty-args
					(ty-rigid-var (name "ok"))
					(ty-rigid-var (name "err")))))
		(alias (type "Person")
			(ty-header (name "Person")))
		(alias (type "MapFn(a, b)")
			(ty-header (name "MapFn")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "ApiResponse(data)")
			(ty-header (name "ApiResponse")
				(ty-args
					(ty-rigid-var (name "data")))))
		(alias (type "Color")
			(ty-header (name "Color")))
		(alias (type "Container(item)")
			(ty-header (name "Container")
				(ty-args
					(ty-rigid-var (name "item"))))))
	(expressions
		(expr (type "_arg -> UserId"))))
~~~
