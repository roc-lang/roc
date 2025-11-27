# META
~~~ini
description=Type variables and values exist in separate namespaces
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Type variable 'item' introduced in annotation
process : List(item) -> item
process = |list| {
	# value identifier named 'item' is allowed - different namespace from type variable
	item = 42

	# type variable 'item' still refers to the function annotation's type parameter
	result : item
	result = List.first(list).ok_or(item)

	result
}

main! = |_| {}
~~~
# EXPECTED
MISSING METHOD - type_var_mismatch.md:7:9:7:11
# PROBLEMS
**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**type_var_mismatch.md:7:9:7:11:**
```roc
	item = 42
```
	       ^^

The value's type, which does not have a method named **from_numeral**, is:

    _item_

**Hint: ** Did you forget to specify **from_numeral** in the type annotation?

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,CloseCurly,
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
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno (name "process")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "item")))
				(ty-var (raw "item"))))
		(s-decl
			(p-ident (raw "process"))
			(e-lambda
				(args
					(p-ident (raw "list")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "item"))
							(e-int (raw "42")))
						(s-type-anno (name "result")
							(ty-var (raw "item")))
						(s-decl
							(p-ident (raw "result"))
							(e-field-access
								(e-apply
									(e-ident (raw "List.first"))
									(e-ident (raw "list")))
								(e-apply
									(e-ident (raw "ok_or"))
									(e-ident (raw "item")))))
						(e-ident (raw "result"))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "process"))
		(e-lambda
			(args
				(p-assign (ident "list")))
			(e-block
				(s-let
					(p-assign (ident "item"))
					(e-num (value "42")))
				(s-let
					(p-assign (ident "result"))
					(e-dot-access (field "ok_or")
						(receiver
							(e-call
								(e-lookup-external
									(builtin))
								(e-lookup-local
									(p-assign (ident "list")))))
						(args
							(e-lookup-local
								(p-assign (ident "item"))))))
				(e-lookup-local
					(p-assign (ident "result")))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "item")))
				(ty-rigid-var-lookup (ty-rigid-var (name "item"))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(item) -> item"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "List(item) -> item"))
		(expr (type "_arg -> {}"))))
~~~
