# META
~~~ini
description=Example of a more complex nominal tag union with two payload types
type=file
~~~
# SOURCE
~~~roc
module [Result, ok, is_ok]

Result(ok, err) := [Ok(a), Err(b)]

ok : a -> Result(a, b)
ok = |a| Result.Ok(a)

is_ok : Result(ok, err) -> Bool
is_ok = |result| match result {
    Result.Ok(_) => True
    Result.Err(_) => False
}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Ok(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_payload_two.md:6:16:6:20:**
```roc
ok = |a| Result.Ok(a)
```
               ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Ok(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_payload_two.md:10:11:10:15:**
```roc
    Result.Ok(_) => True
```
          ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Err(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_payload_two.md:11:11:11:16:**
```roc
    Result.Err(_) => False
```
          ^^^^^


**UNDECLARED TYPE VARIABLE**
The type variable ``a`` is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**nominal_tag_payload_two.md:3:24:3:25:**
```roc
Result(ok, err) := [Ok(a), Err(b)]
```
                       ^


**UNDECLARED TYPE VARIABLE**
The type variable ``b`` is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**nominal_tag_payload_two.md:3:32:3:33:**
```roc
Result(ok, err) := [Ok(a), Err(b)]
```
                               ^


**TYPE REDECLARED**
The type ``Result`` is being redeclared.

The redeclaration is here:
**nominal_tag_payload_two.md:3:1:5:3:**
```roc
Result(ok, err) := [Ok(a), Err(b)]

ok : a -> Result(a, b)
```

But ``Result`` was already declared here:
**nominal_tag_payload_two.md:1:1:1:1:**
```roc
module [Result, ok, is_ok]
```



**UNUSED VARIABLE**
Variable ``a`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_a` to suppress this warning.
The unused variable is declared here:
**nominal_tag_payload_two.md:6:7:6:8:**
```roc
ok = |a| Result.Ok(a)
```
      ^


**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:15),Comma(1:15-1:16),LowerIdent(1:17-1:19),Comma(1:19-1:20),LowerIdent(1:21-1:26),CloseSquare(1:26-1:27),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:7),NoSpaceOpenRound(3:7-3:8),LowerIdent(3:8-3:10),Comma(3:10-3:11),LowerIdent(3:12-3:15),CloseRound(3:15-3:16),OpColonEqual(3:17-3:19),OpenSquare(3:20-3:21),UpperIdent(3:21-3:23),NoSpaceOpenRound(3:23-3:24),LowerIdent(3:24-3:25),CloseRound(3:25-3:26),Comma(3:26-3:27),UpperIdent(3:28-3:31),NoSpaceOpenRound(3:31-3:32),LowerIdent(3:32-3:33),CloseRound(3:33-3:34),CloseSquare(3:34-3:35),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:3),OpColon(5:4-5:5),LowerIdent(5:6-5:7),OpArrow(5:8-5:10),UpperIdent(5:11-5:17),NoSpaceOpenRound(5:17-5:18),LowerIdent(5:18-5:19),Comma(5:19-5:20),LowerIdent(5:21-5:22),CloseRound(5:22-5:23),Newline(1:1-1:1),
LowerIdent(6:1-6:3),OpAssign(6:4-6:5),OpBar(6:6-6:7),LowerIdent(6:7-6:8),OpBar(6:8-6:9),UpperIdent(6:10-6:16),NoSpaceDotUpperIdent(6:16-6:19),NoSpaceOpenRound(6:19-6:20),LowerIdent(6:20-6:21),CloseRound(6:21-6:22),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(8:1-8:6),OpColon(8:7-8:8),UpperIdent(8:9-8:15),NoSpaceOpenRound(8:15-8:16),LowerIdent(8:16-8:18),Comma(8:18-8:19),LowerIdent(8:20-8:23),CloseRound(8:23-8:24),OpArrow(8:25-8:27),UpperIdent(8:28-8:32),Newline(1:1-1:1),
LowerIdent(9:1-9:6),OpAssign(9:7-9:8),OpBar(9:9-9:10),LowerIdent(9:10-9:16),OpBar(9:16-9:17),KwMatch(9:18-9:23),LowerIdent(9:24-9:30),OpenCurly(9:31-9:32),Newline(1:1-1:1),
UpperIdent(10:5-10:11),NoSpaceDotUpperIdent(10:11-10:14),NoSpaceOpenRound(10:14-10:15),Underscore(10:15-10:16),CloseRound(10:16-10:17),OpFatArrow(10:18-10:20),UpperIdent(10:21-10:25),Newline(1:1-1:1),
UpperIdent(11:5-11:11),NoSpaceDotUpperIdent(11:11-11:15),NoSpaceOpenRound(11:15-11:16),Underscore(11:16-11:17),CloseRound(11:17-11:18),OpFatArrow(11:19-11:21),UpperIdent(11:22-11:27),Newline(1:1-1:1),
CloseCurly(12:1-12:2),EndOfFile(12:2-12:2),
~~~
# PARSE
~~~clojure
(file @1.1-12.2
	(module @1.1-1.27
		(exposes @1.8-1.27
			(exposed-upper-ident (text "Result"))
			(exposed-lower-ident (text "ok"))
			(exposed-lower-ident (text "is_ok"))))
	(statements
		(s-type-decl @3.1-5.3
			(header @3.1-3.16 (name "Result")
				(args
					(ty-var @3.8-3.10 (raw "ok"))
					(ty-var @3.12-3.15 (raw "err"))))
			(ty-tag-union @3.20-3.35
				(tags
					(ty-apply @3.21-3.26
						(ty (name "Ok"))
						(ty-var @3.24-3.25 (raw "a")))
					(ty-apply @3.28-3.34
						(ty (name "Err"))
						(ty-var @3.32-3.33 (raw "b"))))))
		(s-type-anno @5.1-6.3 (name "ok")
			(ty-fn @5.6-5.23
				(ty-var @5.6-5.7 (raw "a"))
				(ty-apply @5.11-5.23
					(ty (name "Result"))
					(ty-var @5.18-5.19 (raw "a"))
					(ty-var @5.21-5.22 (raw "b")))))
		(s-decl @6.1-6.16
			(p-ident @6.1-6.3 (raw "ok"))
			(e-lambda @6.6-6.16
				(args
					(p-ident @6.7-6.8 (raw "a")))
				(e-tag @6.10-6.16 (raw "Result"))))
		(e-malformed @6.16-6.20 (reason "expr_unexpected_token"))
		(e-tuple @6.19-6.22
			(e-ident @6.20-6.21 (qaul "") (raw "a")))
		(s-type-anno @8.1-9.6 (name "is_ok")
			(ty-fn @8.9-8.32
				(ty-apply @8.9-8.24
					(ty (name "Result"))
					(ty-var @8.16-8.18 (raw "ok"))
					(ty-var @8.20-8.23 (raw "err")))
				(ty (name "Bool"))))
		(s-decl @9.1-12.2
			(p-ident @9.1-9.6 (raw "is_ok"))
			(e-lambda @9.9-12.2
				(args
					(p-ident @9.10-9.16 (raw "result")))
				(e-match
					(e-ident @9.24-9.30 (qaul "") (raw "result"))
					(branches
						(branch @10.5-10.15
							(p-tag @10.5-10.11 (raw "Result"))
							(e-malformed @10.11-10.15 (reason "expr_unexpected_token")))
						(branch @10.14-11.11
							(p-tuple @10.14-10.17
								(p-underscore))
							(e-tag @10.21-10.25 (raw "True")))
						(branch @11.5-11.16
							(p-tag @11.5-11.11 (raw "Result"))
							(e-malformed @11.11-11.16 (reason "expr_unexpected_token")))
						(branch @11.15-12.2
							(p-tuple @11.15-11.18
								(p-underscore))
							(e-tag @11.22-11.27 (raw "False")))))))))
~~~
# FORMATTED
~~~roc
module [Result, ok, is_ok]

Result(ok, err) : [Ok(a), Err(b)]

ok : a -> Result(a, b)
ok = |a| Result(a)

is_ok : Result(ok, err) -> Bool
is_ok = |result| match result {
	Result => 	(_) => True
	Result => 	(_) => False
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.3 (ident "ok"))
		(e-lambda @6.6-6.16
			(args
				(p-assign @6.7-6.8 (ident "a")))
			(e-tag @6.10-6.16 (name "Result") (args "TODO")))
		(annotation @6.1-6.3
			(declared-type
				(ty-fn @5.6-5.23 (effectful false)
					(ty-var @5.6-5.7 (name "a"))
					(ty-apply @5.11-5.23 (symbol "Result")
						(ty-var @5.18-5.19 (name "a"))
						(ty-var @5.21-5.22 (name "b")))))))
	(d-let
		(p-assign @9.1-9.6 (ident "is_ok"))
		(e-lambda @9.9-12.2
			(args
				(p-assign @9.10-9.16 (ident "result")))
			(e-match @9.18-12.2
				(match @9.18-12.2
					(cond
						(e-lookup-local @9.24-9.30
							(pattern @9.10-9.16)))
					(branches
						(branch
							(patterns
								(p-applied-tag @10.5-10.11 (degenerate false)))
							(value
								(e-runtime-error (tag "expr_not_canonicalized"))))
						(branch
							(patterns
								(p-tuple @10.14-10.17 (degenerate false)
									(patterns
										(p-underscore @10.15-10.16))))
							(value
								(e-tag @10.21-10.25 (name "True") (args "TODO"))))
						(branch
							(patterns
								(p-applied-tag @11.5-11.11 (degenerate false)))
							(value
								(e-runtime-error (tag "expr_not_canonicalized"))))
						(branch
							(patterns
								(p-tuple @11.15-11.18 (degenerate false)
									(patterns
										(p-underscore @11.16-11.17))))
							(value
								(e-tag @11.22-11.27 (name "False") (args "TODO"))))))))
		(annotation @9.1-9.6
			(declared-type
				(ty-fn @8.9-8.32 (effectful false)
					(ty-apply @8.9-8.24 (symbol "Result")
						(ty-var @8.16-8.18 (name "ok"))
						(ty-var @8.20-8.23 (name "err")))
					(ty @8.28-8.32 (name "Bool"))))))
	(s-type-decl @3.1-5.3
		(ty-header @3.1-3.16 (name "Result")
			(ty-args
				(ty-var @3.8-3.10 (name "ok"))
				(ty-var @3.12-3.15 (name "err"))))
		(ty-tag-union @3.20-3.35
			(ty-apply @3.21-3.26 (symbol "Ok")
				(ty-var @3.24-3.25 (name "a")))
			(ty-apply @3.28-3.34 (symbol "Err")
				(ty-var @3.32-3.33 (name "b"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.3 (type "a -> Error"))
		(patt @9.1-9.6 (type "Error -> Error")))
	(expressions
		(expr @6.6-6.16 (type "a -> Error"))
		(expr @9.9-12.2 (type "Error -> Error"))))
~~~
