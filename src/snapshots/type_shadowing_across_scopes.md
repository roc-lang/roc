# META
~~~ini
description=Type shadowing across scopes should produce warning
type=file
~~~
# SOURCE
~~~roc
module [Result, processData]

Result(a, b) : [Ok(a), Err(b)]

processData : Str -> Str
processData = |data|
    "processed"

# In a nested module scope, redeclare Result
InnerModule : {
    Result : [Success, Failure]
}
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_type_field_name`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_shadowing_across_scopes.md:11:5:11:13:**
```roc
    Result : [Success, Failure]
```


**PARSE ERROR**
A parsing error occurred: `expected_ty_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_shadowing_across_scopes.md:11:24:11:32:**
```roc
    Result : [Success, Failure]
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_shadowing_across_scopes.md:11:31:11:31:**
```roc
    Result : [Success, Failure]
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_shadowing_across_scopes.md:12:1:12:2:**
```roc
}
```


**TYPE REDECLARED**
The type ``Result`` is being redeclared.

The redeclaration is here:
**type_shadowing_across_scopes.md:4:2:6:13:**
```roc

processData : Str -> Str
processData = |data|
```

But ``Result`` was already declared here:
**type_shadowing_across_scopes.md:2:2:2:2:**
```roc

```


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**UNUSED VARIABLE**
Variable ``data`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_data` to suppress this warning.
The unused variable is declared here:
**type_shadowing_across_scopes.md:7:17:7:21:**
```roc
    "processed"
```


**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:15),Comma(1:15-1:16),LowerIdent(1:17-1:28),CloseSquare(1:28-1:29),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:7),NoSpaceOpenRound(3:7-3:8),LowerIdent(3:8-3:9),Comma(3:9-3:10),LowerIdent(3:11-3:12),CloseRound(3:12-3:13),OpColon(3:14-3:15),OpenSquare(3:16-3:17),UpperIdent(3:17-3:19),NoSpaceOpenRound(3:19-3:20),LowerIdent(3:20-3:21),CloseRound(3:21-3:22),Comma(3:22-3:23),UpperIdent(3:24-3:27),NoSpaceOpenRound(3:27-3:28),LowerIdent(3:28-3:29),CloseRound(3:29-3:30),CloseSquare(3:30-3:31),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:12),OpColon(5:13-5:14),UpperIdent(5:15-5:18),OpArrow(5:19-5:21),UpperIdent(5:22-5:25),Newline(1:1-1:1),
LowerIdent(6:1-6:12),OpAssign(6:13-6:14),OpBar(6:15-6:16),LowerIdent(6:16-6:20),OpBar(6:20-6:21),Newline(1:1-1:1),
StringStart(7:5-7:6),StringPart(7:6-7:15),StringEnd(7:15-7:16),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(9:2-9:45),
UpperIdent(10:1-10:12),OpColon(10:13-10:14),OpenCurly(10:15-10:16),Newline(1:1-1:1),
UpperIdent(11:5-11:11),OpColon(11:12-11:13),OpenSquare(11:14-11:15),UpperIdent(11:15-11:22),Comma(11:22-11:23),UpperIdent(11:24-11:31),CloseSquare(11:31-11:32),Newline(1:1-1:1),
CloseCurly(12:1-12:2),EndOfFile(12:2-12:2),
~~~
# PARSE
~~~clojure
(file (1:1-12:2)
	(module (1:1-1:29)
		(exposes (1:8-1:29)
			(exposed_item (upper_ident "Result"))
			(exposed_item (lower_ident "processData"))))
	(statements
		(type_decl (3:1-5:12)
			(header (3:1-3:13)
				"Result"
				(args
					(ty_var (3:8-3:9) "a")
					(ty_var (3:11-3:12) "b")))
			(tag_union (3:16-3:31)
				(tags
					(apply (3:17-3:22)
						(ty "Ok")
						(ty_var (3:20-3:21) "a"))
					(apply (3:24-3:30)
						(ty "Err")
						(ty_var (3:28-3:29) "b")))))
		(type_anno (5:1-6:12)
			"processData"
			(fn (5:15-5:25)
				(ty "Str")
				(ty "Str")))
		(decl (6:1-7:16)
			(ident (6:1-6:12) "processData")
			(lambda (6:15-7:16)
				(args (ident (6:16-6:20) "data"))
				(string (7:5-7:16) (string_part (7:6-7:15) "processed"))))
		(type_decl (10:1-11:32)
			(header (10:1-10:12) "InnerModule" (args))
			(malformed_expr (11:24-11:32) "expected_ty_close_curly_or_comma"))
		(malformed_expr (1:1-1:1) "expr_unexpected_token")
		(malformed_expr (12:1-12:2) "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
module [Result, processData]

Result(a, b) : [Ok(a), Err(b)]

processData : Str -> Str
processData = |data|
	"processed"

# In a nested module scope, redeclare Result
InnerModule : 

~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (6:1-6:12)
				(pid 89)
				(ident "processData")))
		(def_expr
			(e_lambda (6:15-7:16)
				(args
					(p_assign (6:16-6:20)
						(pid 90)
						(ident "data")))
				(e_string (7:5-7:16) (e_literal (7:6-7:15) "processed"))))
		(annotation (6:1-6:12)
			(signature 98)
			(declared_type
				(fn (5:15-5:25)
					(ty (5:15-5:18) "Str")
					(ty (5:22-5:25) "Str")
					"false"))))
	(s_type_decl (3:1-5:12)
		(type_header (3:1-3:13)
			"Result"
			(args
				(ty_var (3:8-3:9) "a")
				(ty_var (3:11-3:12) "b")))
		(tag_union (3:16-3:31)
			(apply (3:17-3:22)
				"Ok"
				(ty_var (3:20-3:21) "a"))
			(apply (3:24-3:30)
				"Err"
				(ty_var (3:28-3:29) "b"))))
	(s_type_decl (10:1-11:32)
		(type_header (10:1-10:12) "InnerModule")
		(malformed_type_anno (11:24-11:32))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "processData" 100 (type "*")))
	(expressions
		(expr (6:15-7:16) 93 (type "*"))))
~~~