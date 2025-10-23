# META
~~~ini
description=Complex tag union types with multiple variants and nesting
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Simple tag union with no-argument tags
Status : [Loading, Complete, Failed]

# Tag union with mixed argument types
Result : [Success(Str), Error(Str), Warning(Str, I32)]

# Nested tag unions
Response : [Ok(Result), NetworkError, ParseError]

# Multiple tag unions using similar tag names
UserState : [Active(Str), Inactive, Suspended(Str)]
ConnectionState : [Active, Disconnected, Connecting(Str)]

# Function using tag unions
processResult : Result -> Str
processResult = |_result| "processed"

# Function with nested tag union
handleResponse : Response -> Str
handleResponse = |_response| "handled"

main! = |_| {}
~~~
# EXPECTED
TYPE REDECLARED - type_tag_union_complex.md:7:1:7:55
# PROBLEMS
**TYPE REDECLARED**
The type _Result_ is being redeclared.

The redeclaration is here:
**type_tag_union_complex.md:7:1:7:55:**
```roc
Result : [Success(Str), Error(Str), Warning(Str, I32)]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

But _Result_ was already declared here:
**type_tag_union_complex.md:1:1:1:1:**
```roc
app [main!] { pf: platform "../basic-cli/main.roc" }
```
^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
UpperIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,CloseSquare,
UpperIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
UpperIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,StringStart,StringPart,StringEnd,
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
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-decl
			(header (name "Status")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Loading"))
					(ty (name "Complete"))
					(ty (name "Failed")))))
		(s-type-decl
			(header (name "Result")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Success"))
						(ty (name "Str")))
					(ty-apply
						(ty (name "Error"))
						(ty (name "Str")))
					(ty-apply
						(ty (name "Warning"))
						(ty (name "Str"))
						(ty (name "I32"))))))
		(s-type-decl
			(header (name "Response")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Ok"))
						(ty (name "Result")))
					(ty (name "NetworkError"))
					(ty (name "ParseError")))))
		(s-type-decl
			(header (name "UserState")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Active"))
						(ty (name "Str")))
					(ty (name "Inactive"))
					(ty-apply
						(ty (name "Suspended"))
						(ty (name "Str"))))))
		(s-type-decl
			(header (name "ConnectionState")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Active"))
					(ty (name "Disconnected"))
					(ty-apply
						(ty (name "Connecting"))
						(ty (name "Str"))))))
		(s-type-anno (name "processResult")
			(ty-fn
				(ty (name "Result"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "processResult"))
			(e-lambda
				(args
					(p-ident (raw "_result")))
				(e-string
					(e-string-part (raw "processed")))))
		(s-type-anno (name "handleResponse")
			(ty-fn
				(ty (name "Response"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "handleResponse"))
			(e-lambda
				(args
					(p-ident (raw "_response")))
				(e-string
					(e-string-part (raw "handled")))))
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
		(p-assign (ident "processResult"))
		(e-lambda
			(args
				(p-assign (ident "_result")))
			(e-string
				(e-literal (string "processed"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Result") (external-module "Result"))
				(ty-lookup (name "Str") (external-module "Str")))))
	(d-let
		(p-assign (ident "handleResponse"))
		(e-lambda
			(args
				(p-assign (ident "_response")))
			(e-string
				(e-literal (string "handled"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Response") (local))
				(ty-lookup (name "Str") (external-module "Str")))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record)))
	(s-alias-decl
		(ty-header (name "Status"))
		(ty-tag-union
			(ty-tag-name (name "Loading"))
			(ty-tag-name (name "Complete"))
			(ty-tag-name (name "Failed"))))
	(s-alias-decl
		(ty-header (name "Result"))
		(ty-tag-union
			(ty-tag-name (name "Success")
				(ty-lookup (name "Str") (external-module "Str")))
			(ty-tag-name (name "Error")
				(ty-lookup (name "Str") (external-module "Str")))
			(ty-tag-name (name "Warning")
				(ty-lookup (name "Str") (external-module "Str"))
				(ty-lookup (name "I32") (builtin)))))
	(s-alias-decl
		(ty-header (name "Response"))
		(ty-tag-union
			(ty-tag-name (name "Ok")
				(ty-lookup (name "Result") (external-module "Result")))
			(ty-tag-name (name "NetworkError"))
			(ty-tag-name (name "ParseError"))))
	(s-alias-decl
		(ty-header (name "UserState"))
		(ty-tag-union
			(ty-tag-name (name "Active")
				(ty-lookup (name "Str") (external-module "Str")))
			(ty-tag-name (name "Inactive"))
			(ty-tag-name (name "Suspended")
				(ty-lookup (name "Str") (external-module "Str")))))
	(s-alias-decl
		(ty-header (name "ConnectionState"))
		(ty-tag-union
			(ty-tag-name (name "Active"))
			(ty-tag-name (name "Disconnected"))
			(ty-tag-name (name "Connecting")
				(ty-lookup (name "Str") (external-module "Str"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Result(ok, err) -> Str"))
		(patt (type "Response -> Str"))
		(patt (type "_arg -> {}")))
	(type_decls
		(alias (type "Status")
			(ty-header (name "Status")))
		(alias (type "Result")
			(ty-header (name "Result")))
		(alias (type "Response")
			(ty-header (name "Response")))
		(alias (type "UserState")
			(ty-header (name "UserState")))
		(alias (type "ConnectionState")
			(ty-header (name "ConnectionState"))))
	(expressions
		(expr (type "Result(ok, err) -> Str"))
		(expr (type "Response -> Str"))
		(expr (type "_arg -> {}"))))
~~~
