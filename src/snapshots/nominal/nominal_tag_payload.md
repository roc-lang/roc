# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module [Maybe, some, none]

Maybe(a) := [Some(a), None]

some : a -> Maybe(a)
some = |a| Maybe.Some(a)

none : Maybe(a)
none = Maybe.None
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Some(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_payload.md:6:17:6:23:**
```roc
some = |a| Maybe.Some(a)
```
                ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.None** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_payload.md:9:13:9:18:**
```roc
none = Maybe.None
```
            ^^^^^


**UNUSED VARIABLE**
Variable ``a`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_a` to suppress this warning.
The unused variable is declared here:
**nominal_tag_payload.md:6:9:6:10:**
```roc
some = |a| Maybe.Some(a)
```
        ^


**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:14),Comma(1:14-1:15),LowerIdent(1:16-1:20),Comma(1:20-1:21),LowerIdent(1:22-1:26),CloseSquare(1:26-1:27),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:6),NoSpaceOpenRound(3:6-3:7),LowerIdent(3:7-3:8),CloseRound(3:8-3:9),OpColonEqual(3:10-3:12),OpenSquare(3:13-3:14),UpperIdent(3:14-3:18),NoSpaceOpenRound(3:18-3:19),LowerIdent(3:19-3:20),CloseRound(3:20-3:21),Comma(3:21-3:22),UpperIdent(3:23-3:27),CloseSquare(3:27-3:28),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:5),OpColon(5:6-5:7),LowerIdent(5:8-5:9),OpArrow(5:10-5:12),UpperIdent(5:13-5:18),NoSpaceOpenRound(5:18-5:19),LowerIdent(5:19-5:20),CloseRound(5:20-5:21),Newline(1:1-1:1),
LowerIdent(6:1-6:5),OpAssign(6:6-6:7),OpBar(6:8-6:9),LowerIdent(6:9-6:10),OpBar(6:10-6:11),UpperIdent(6:12-6:17),NoSpaceDotUpperIdent(6:17-6:22),NoSpaceOpenRound(6:22-6:23),LowerIdent(6:23-6:24),CloseRound(6:24-6:25),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(8:1-8:5),OpColon(8:6-8:7),UpperIdent(8:8-8:13),NoSpaceOpenRound(8:13-8:14),LowerIdent(8:14-8:15),CloseRound(8:15-8:16),Newline(1:1-1:1),
LowerIdent(9:1-9:5),OpAssign(9:6-9:7),UpperIdent(9:8-9:13),NoSpaceDotUpperIdent(9:13-9:18),EndOfFile(9:18-9:18),
~~~
# PARSE
~~~clojure
(file @1.1-9.18
	(module @1.1-1.27
		(exposes @1.8-1.27
			(exposed-upper-ident (text "Maybe"))
			(exposed-lower-ident (text "some"))
			(exposed-lower-ident (text "none"))))
	(statements
		(s-type-decl @3.1-5.5
			(header @3.1-3.9 (name "Maybe")
				(args
					(ty-var @3.7-3.8 (raw "a"))))
			(ty-tag-union @3.13-3.28
				(tags
					(ty-apply @3.14-3.21
						(ty (name "Some"))
						(ty-var @3.19-3.20 (raw "a")))
					(ty (name "None")))))
		(s-type-anno @5.1-6.5 (name "some")
			(ty-fn @5.8-5.21
				(ty-var @5.8-5.9 (raw "a"))
				(ty-apply @5.13-5.21
					(ty (name "Maybe"))
					(ty-var @5.19-5.20 (raw "a")))))
		(s-decl @6.1-6.17
			(p-ident @6.1-6.5 (raw "some"))
			(e-lambda @6.8-6.17
				(args
					(p-ident @6.9-6.10 (raw "a")))
				(e-tag @6.12-6.17 (raw "Maybe"))))
		(e-malformed @6.17-6.23 (reason "expr_unexpected_token"))
		(e-tuple @6.22-6.25
			(e-ident @6.23-6.24 (qaul "") (raw "a")))
		(s-type-anno @8.1-9.5 (name "none")
			(ty-apply @8.8-8.16
				(ty (name "Maybe"))
				(ty-var @8.14-8.15 (raw "a"))))
		(s-decl @9.1-9.13
			(p-ident @9.1-9.5 (raw "none"))
			(e-tag @9.8-9.13 (raw "Maybe")))
		(e-malformed @9.13-9.18 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [Maybe, some, none]

Maybe(a) : [Some(a), None]

some : a -> Maybe(a)
some = |a| Maybe(a)

none : Maybe(a)
none = Maybe
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.5 (ident "some"))
		(e-lambda @6.8-6.17
			(args
				(p-assign @6.9-6.10 (ident "a")))
			(e-tag @6.12-6.17 (name "Maybe") (args "TODO")))
		(annotation @6.1-6.5
			(declared-type
				(ty-fn @5.8-5.21 (effectful false)
					(ty-var @5.8-5.9 (name "a"))
					(ty-apply @5.13-5.21 (symbol "Maybe")
						(ty-var @5.19-5.20 (name "a")))))))
	(d-let
		(p-assign @9.1-9.5 (ident "none"))
		(e-tag @9.8-9.13 (name "Maybe") (args "TODO"))
		(annotation @9.1-9.5
			(declared-type
				(ty-apply @8.8-8.16 (symbol "Maybe")
					(ty-var @8.14-8.15 (name "a"))))))
	(s-type-decl @3.1-5.5
		(ty-header @3.1-3.9 (name "Maybe")
			(ty-args
				(ty-var @3.7-3.8 (name "a"))))
		(ty-tag-union @3.13-3.28
			(ty-apply @3.14-3.21 (symbol "Some")
				(ty-var @3.19-3.20 (name "a")))
			(ty @3.23-3.27 (name "None")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.5 (type "a -> [Maybe]*"))
		(patt @9.1-9.5 (type "[Maybe]*")))
	(expressions
		(expr @6.8-6.17 (type "a -> [Maybe]*"))
		(expr @9.8-9.13 (type "[Maybe]*"))))
~~~
