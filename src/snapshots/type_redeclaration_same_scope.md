# META
~~~ini
description=Type redeclaration in same scope should produce error
type=file
~~~
# SOURCE
~~~roc
module [Maybe]

Maybe(a) : [Some(a), None]
Maybe(a) : [Ok(a), Err]
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - type_redeclaration_same_scope.md:5:1:5:3
UNEXPECTED TOKEN IN EXPRESSION - type_redeclaration_same_scope.md:5:2:5:4
UNEXPECTED TOKEN IN EXPRESSION - type_redeclaration_same_scope.md:5:3:5:4
TYPE REDECLARED - type_redeclaration_same_scope.md:4:1:4:24
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_redeclaration_same_scope.md:5:1:5:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_redeclaration_same_scope.md:5:2:5:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_redeclaration_same_scope.md:5:3:5:4:**
```roc
~~~
```
  ^


**TYPE REDECLARED**
The type ``Maybe`` is being redeclared.

The redeclaration is here:
**type_redeclaration_same_scope.md:4:1:4:24:**
```roc
Maybe(a) : [Ok(a), Err]
```
^^^^^^^^^^^^^^^^^^^^^^^

But ``Maybe`` was already declared here:
**type_redeclaration_same_scope.md:3:1:3:27:**
```roc
Maybe(a) : [Some(a), None]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:14),CloseSquare(1:14-1:15),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:6),NoSpaceOpenRound(3:6-3:7),LowerIdent(3:7-3:8),CloseRound(3:8-3:9),OpColon(3:10-3:11),OpenSquare(3:12-3:13),UpperIdent(3:13-3:17),NoSpaceOpenRound(3:17-3:18),LowerIdent(3:18-3:19),CloseRound(3:19-3:20),Comma(3:20-3:21),UpperIdent(3:22-3:26),CloseSquare(3:26-3:27),Newline(1:1-1:1),
UpperIdent(4:1-4:6),NoSpaceOpenRound(4:6-4:7),LowerIdent(4:7-4:8),CloseRound(4:8-4:9),OpColon(4:10-4:11),OpenSquare(4:12-4:13),UpperIdent(4:13-4:15),NoSpaceOpenRound(4:15-4:16),LowerIdent(4:16-4:17),CloseRound(4:17-4:18),Comma(4:18-4:19),UpperIdent(4:20-4:23),CloseSquare(4:23-4:24),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(file @1.1-5.4
	(module @1.1-1.15
		(exposes @1.8-1.15
			(exposed-upper-ident (text "Maybe"))))
	(statements
		(s-type-decl @3.1-3.27
			(header @3.1-3.9 (name "Maybe")
				(args
					(ty-var @3.7-3.8 (raw "a"))))
			(ty-tag-union @3.12-3.27
				(tags
					(ty-apply @3.13-3.20
						(ty @3.13-3.17 (name "Some"))
						(ty-var @3.18-3.19 (raw "a")))
					(ty @3.22-3.26 (name "None")))))
		(s-type-decl @4.1-4.24
			(header @4.1-4.9 (name "Maybe")
				(args
					(ty-var @4.7-4.8 (raw "a"))))
			(ty-tag-union @4.12-4.24
				(tags
					(ty-apply @4.13-4.18
						(ty @4.13-4.15 (name "Ok"))
						(ty-var @4.16-4.17 (raw "a")))
					(ty @4.20-4.23 (name "Err")))))
		(e-malformed @5.1-5.3 (reason "expr_unexpected_token"))
		(e-malformed @5.2-5.4 (reason "expr_unexpected_token"))
		(e-malformed @5.3-5.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [Maybe]

Maybe(a) : [Some(a), None]
Maybe(a) : [Ok(a), Err]

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-3.27 (where "TODO")
		(ty-header @3.1-3.9 (name "Maybe")
			(ty-args
				(ty-var @3.7-3.8 (name "a"))))
		(ty-tag-union @3.12-3.27
			(ty-apply @3.13-3.20 (symbol "Some")
				(ty-var @3.18-3.19 (name "a")))
			(ty @3.22-3.26 (name "None"))))
	(s-alias-decl @4.1-4.24 (where "TODO")
		(ty-header @4.1-4.9 (name "Maybe")
			(ty-args
				(ty-var @4.7-4.8 (name "a"))))
		(ty-tag-union @4.12-4.24
			(ty-apply @4.13-4.18 (symbol "Ok")
				(ty-var @4.16-4.17 (name "a")))
			(ty @4.20-4.23 (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
