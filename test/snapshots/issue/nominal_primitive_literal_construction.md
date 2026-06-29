# META
~~~ini
description=Primitive-backed nominal types accept matching number/string literals
type=snippet
~~~
# SOURCE
~~~roc
UserId := U64

uid : UserId
uid = 0

Token := Str

token : Token
token = "abc"

GoodBase := Str
GoodDerived := GoodBase

goodValue : GoodDerived
goodValue = "test"
~~~
# EXPECTED
TYPE MISMATCH - nominal_primitive_literal_construction.md:4:7:4:8
TYPE MISMATCH - nominal_primitive_literal_construction.md:9:9:9:14
TYPE MISMATCH - nominal_primitive_literal_construction.md:15:13:15:19
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This number is being used where a non-number type is ──────┐
└┬──────────────┘  needed.                                                    │
 │                                                                            │
 │  uid = 0                                                                   │
 │        ‾                                                                   │
 └───────────────────────────── nominal_primitive_literal_construction.md:4:7 ┘

    Other code expects this to have the type:

        UserId


┌───────────────┐
│ TYPE MISMATCH ├─ This string literal is being used where a non-string ──────┐
└┬──────────────┘  type is needed.                                            │
 │                                                                            │
 │  token = "abc"                                                             │
 │          ‾‾‾‾‾                                                             │
 └───────────────────────────── nominal_primitive_literal_construction.md:9:9 ┘

    The type was determined to be:

        Token


┌───────────────┐
│ TYPE MISMATCH ├─ This string literal is being used where a non-string ──────┐
└┬──────────────┘  type is needed.                                            │
 │                                                                            │
 │  goodValue = "test"                                                        │
 │              ‾‾‾‾‾‾                                                        │
 └─────────────────────────── nominal_primitive_literal_construction.md:15:13 ┘

    The type was determined to be:

        GoodDerived

# TOKENS
~~~zig
UpperIdent,OpColonEqual,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
UpperIdent,OpColonEqual,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
UpperIdent,OpColonEqual,UpperIdent,
UpperIdent,OpColonEqual,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "UserId")
				(args))
			(ty (name "U64")))
		(s-type-anno (name "uid")
			(ty (name "UserId")))
		(s-decl
			(p-ident (raw "uid"))
			(e-int (raw "0")))
		(s-type-decl
			(header (name "Token")
				(args))
			(ty (name "Str")))
		(s-type-anno (name "token")
			(ty (name "Token")))
		(s-decl
			(p-ident (raw "token"))
			(e-string
				(e-string-part (raw "abc"))))
		(s-type-decl
			(header (name "GoodBase")
				(args))
			(ty (name "Str")))
		(s-type-decl
			(header (name "GoodDerived")
				(args))
			(ty (name "GoodBase")))
		(s-type-anno (name "goodValue")
			(ty (name "GoodDerived")))
		(s-decl
			(p-ident (raw "goodValue"))
			(e-string
				(e-string-part (raw "test"))))))
~~~
# FORMATTED
~~~roc
UserId := U64

uid : UserId
uid = 0

Token := Str

token : Token
token = "abc"

GoodBase := Str

GoodDerived := GoodBase

goodValue : GoodDerived
goodValue = "test"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "uid"))
		(e-num (value "0"))
		(annotation
			(ty-lookup (name "UserId") (local))))
	(d-let
		(p-assign (ident "token"))
		(e-string
			(e-literal (string "abc")))
		(annotation
			(ty-lookup (name "Token") (local))))
	(d-let
		(p-assign (ident "goodValue"))
		(e-string
			(e-literal (string "test")))
		(annotation
			(ty-lookup (name "GoodDerived") (local))))
	(s-nominal-decl
		(ty-header (name "UserId"))
		(ty-lookup (name "U64") (builtin)))
	(s-nominal-decl
		(ty-header (name "Token"))
		(ty-lookup (name "Str") (builtin)))
	(s-nominal-decl
		(ty-header (name "GoodBase"))
		(ty-lookup (name "Str") (builtin)))
	(s-nominal-decl
		(ty-header (name "GoodDerived"))
		(ty-lookup (name "GoodBase") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "UserId"))
		(patt (type "Token"))
		(patt (type "GoodDerived")))
	(type_decls
		(nominal (type "UserId")
			(ty-header (name "UserId")))
		(nominal (type "Token")
			(ty-header (name "Token")))
		(nominal (type "GoodBase")
			(ty-header (name "GoodBase")))
		(nominal (type "GoodDerived")
			(ty-header (name "GoodDerived"))))
	(expressions
		(expr (type "UserId"))
		(expr (type "Token"))
		(expr (type "GoodDerived"))))
~~~
