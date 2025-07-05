# META
~~~ini
description=Multiple unqualified type declarations
type=file
~~~
# SOURCE
~~~roc
module []

FirstType : U64
SecondType : Str
ThirdType : List(U8)
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - file_multiple_type_decls.md:6:1:6:3
UNEXPECTED TOKEN IN EXPRESSION - file_multiple_type_decls.md:6:2:6:4
UNEXPECTED TOKEN IN EXPRESSION - file_multiple_type_decls.md:6:3:6:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**file_multiple_type_decls.md:6:1:6:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**file_multiple_type_decls.md:6:2:6:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**file_multiple_type_decls.md:6:3:6:4:**
```roc
~~~
```
  ^


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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:10),OpColon(3:11-3:12),UpperIdent(3:13-3:16),Newline(1:1-1:1),
UpperIdent(4:1-4:11),OpColon(4:12-4:13),UpperIdent(4:14-4:17),Newline(1:1-1:1),
UpperIdent(5:1-5:10),OpColon(5:11-5:12),UpperIdent(5:13-5:17),NoSpaceOpenRound(5:17-5:18),UpperIdent(5:18-5:20),CloseRound(5:20-5:21),Newline(1:1-1:1),
MalformedUnknownToken(6:1-6:2),MalformedUnknownToken(6:2-6:3),MalformedUnknownToken(6:3-6:4),EndOfFile(6:4-6:4),
~~~
# PARSE
~~~clojure
(file @1.1-6.4
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.16
			(header @3.1-3.10 (name "FirstType")
				(args))
			(ty @3.13-3.16 (name "U64")))
		(s-type-decl @4.1-4.17
			(header @4.1-4.11 (name "SecondType")
				(args))
			(ty @4.14-4.17 (name "Str")))
		(s-type-decl @5.1-5.21
			(header @5.1-5.10 (name "ThirdType")
				(args))
			(ty-apply @5.13-5.21
				(ty @5.13-5.17 (name "List"))
				(ty @5.18-5.20 (name "U8"))))
		(e-malformed @6.1-6.3 (reason "expr_unexpected_token"))
		(e-malformed @6.2-6.4 (reason "expr_unexpected_token"))
		(e-malformed @6.3-6.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []

FirstType : U64
SecondType : Str
ThirdType : List(U8)

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-3.16 (where "TODO")
		(ty-header @3.1-3.10 (name "FirstType"))
		(ty @3.13-3.16 (name "U64")))
	(s-alias-decl @4.1-4.17 (where "TODO")
		(ty-header @4.1-4.11 (name "SecondType"))
		(ty @4.14-4.17 (name "Str")))
	(s-alias-decl @5.1-5.21 (where "TODO")
		(ty-header @5.1-5.10 (name "ThirdType"))
		(ty-apply @5.13-5.21 (symbol "List")
			(ty @5.18-5.20 (name "U8")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
