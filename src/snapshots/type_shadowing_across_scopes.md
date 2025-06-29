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
**type_shadowing_across_scopes.md:3:1:5:12:**
```roc
Result(a, b) : [Ok(a), Err(b)]

processData : Str -> Str
```

But ``Result`` was already declared here:
**type_shadowing_across_scopes.md:1:1:1:1:**
```roc
module [Result, processData]
```


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**UNUSED VARIABLE**
Variable ``data`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_data` to suppress this warning.
The unused variable is declared here:
**type_shadowing_across_scopes.md:6:16:6:20:**
```roc
processData = |data|
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
(file @1.1-12.2
	(module @1.1-1.29
		(exposes @1.8-1.29
			(exposed-upper-ident (text "Result"))
			(exposed-lower-ident (text "processData"))))
	(statements
		(s-type-decl @3.1-5.12
			(header @3.1-3.13 (name "Result")
				(args
					(ty-var @3.8-3.9 (raw "a"))
					(ty-var @3.11-3.12 (raw "b"))))
			(ty-tag-union @3.16-3.31
				(tags
					(ty-apply @3.17-3.22
						(ty (name "Ok"))
						(ty-var @3.20-3.21 (raw "a")))
					(ty-apply @3.24-3.30
						(ty (name "Err"))
						(ty-var @3.28-3.29 (raw "b"))))))
		(s-type-anno @5.1-6.12 (name "processData")
			(ty-fn @5.15-5.25
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl @6.1-7.16
			(p-ident @6.1-6.12 (raw "processData"))
			(e-lambda @6.15-7.16
				(args
					(p-ident @6.16-6.20 (raw "data")))
				(e-string @7.5-7.16
					(e-string-part @7.6-7.15 (raw "processed")))))
		(s-type-decl @10.1-11.32
			(header @10.1-10.12 (name "InnerModule")
				(args))
			(ty-malformed @11.24-11.32 (tag "expected_ty_close_curly_or_comma")))
		(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
		(e-malformed @12.1-12.2 (reason "expr_unexpected_token"))))
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
(can-ir
	(d-let (id 103)
		(p-assign @6.1-6.12 (ident "processData") (id 90))
		(e-lambda @6.15-7.16 (id 95)
			(args
				(p-assign @6.16-6.20 (ident "data") (id 91)))
			(e-string @7.5-7.16
				(e-literal @7.6-7.15 (string "processed"))))
		(annotation @6.1-6.12 (signature 101) (id 102)
			(declared-type
				(ty-fn @5.15-5.25 (effectful false)
					(ty @5.15-5.18 (name "Str"))
					(ty @5.22-5.25 (name "Str"))))))
	(s-type-decl @3.1-5.12 (id 81)
		(ty-header @3.1-3.13 (name "Result")
			(ty-args
				(ty-var @3.8-3.9 (name "a"))
				(ty-var @3.11-3.12 (name "b"))))
		(ty-tag-union @3.16-3.31
			(ty-apply @3.17-3.22 (symbol "Ok")
				(ty-var @3.20-3.21 (name "a")))
			(ty-apply @3.24-3.30 (symbol "Err")
				(ty-var @3.28-3.29 (name "b")))))
	(s-type-decl @10.1-11.32 (id 86)
		(ty-header @10.1-10.12 (name "InnerModule"))
		(ty-malformed @11.24-11.32)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "processData") (def_var 103) (type "Str -> Str")))
	(expressions
		(expr @6.15-7.16 (type "Str -> Str"))))
~~~
