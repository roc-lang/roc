# META
~~~ini
description=Type shadowing across scopes should produce warning
type=file
~~~
# SOURCE
~~~roc
Result(a, b) : [Ok(a), Err(b)]

processData : Str -> Str
processData = |data|
    "processed"

# In a nested module scope, redeclare Result
InnerModule : {
    Result : [Success, Failure]
}
~~~
# EXPECTED
PARSE ERROR - type_shadowing_across_scopes.md:9:5:9:11
PARSE ERROR - type_shadowing_across_scopes.md:9:24:9:31
PARSE ERROR - type_shadowing_across_scopes.md:9:31:9:32
PARSE ERROR - type_shadowing_across_scopes.md:10:1:10:2
TYPE REDECLARED - type_shadowing_across_scopes.md:1:1:1:31
MALFORMED TYPE - type_shadowing_across_scopes.md:9:24:9:31
TYPE MODULE MISSING MATCHING TYPE - type_shadowing_across_scopes.md:1:1:10:2
UNUSED VARIABLE - type_shadowing_across_scopes.md:4:16:4:20
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_type_field_name`
This is an unexpected parsing error. Please check your syntax.

**type_shadowing_across_scopes.md:9:5:9:11:**
```roc
    Result : [Success, Failure]
```
    ^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_ty_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

**type_shadowing_across_scopes.md:9:24:9:31:**
```roc
    Result : [Success, Failure]
```
                       ^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**type_shadowing_across_scopes.md:9:31:9:32:**
```roc
    Result : [Success, Failure]
```
                              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**type_shadowing_across_scopes.md:10:1:10:2:**
```roc
}
```
^


**TYPE REDECLARED**
The type _Result_ is being redeclared.

The redeclaration is here:
**type_shadowing_across_scopes.md:1:1:1:31:**
```roc
Result(a, b) : [Ok(a), Err(b)]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

But _Result_ was already declared here:
**type_shadowing_across_scopes.md:1:1:1:1:**
```roc
Result(a, b) : [Ok(a), Err(b)]
```
^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**type_shadowing_across_scopes.md:9:24:9:31:**
```roc
    Result : [Success, Failure]
```
                       ^^^^^^^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `type_shadowing_across_scopes.roc`, but no top-level type declaration named `type_shadowing_across_scopes` was found.

Add either:
`type_shadowing_across_scopes := ...` (nominal type)
or:
`type_shadowing_across_scopes : ...` (type alias)
**type_shadowing_across_scopes.md:1:1:10:2:**
```roc
Result(a, b) : [Ok(a), Err(b)]

processData : Str -> Str
processData = |data|
    "processed"

# In a nested module scope, redeclare Result
InnerModule : {
    Result : [Success, Failure]
}
```


**UNUSED VARIABLE**
Variable `data` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_data` to suppress this warning.
The unused variable is declared here:
**type_shadowing_across_scopes.md:4:16:4:20:**
```roc
processData = |data|
```
               ^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:7),NoSpaceOpenRound(1:7-1:8),LowerIdent(1:8-1:9),Comma(1:9-1:10),LowerIdent(1:11-1:12),CloseRound(1:12-1:13),OpColon(1:14-1:15),OpenSquare(1:16-1:17),UpperIdent(1:17-1:19),NoSpaceOpenRound(1:19-1:20),LowerIdent(1:20-1:21),CloseRound(1:21-1:22),Comma(1:22-1:23),UpperIdent(1:24-1:27),NoSpaceOpenRound(1:27-1:28),LowerIdent(1:28-1:29),CloseRound(1:29-1:30),CloseSquare(1:30-1:31),
LowerIdent(3:1-3:12),OpColon(3:13-3:14),UpperIdent(3:15-3:18),OpArrow(3:19-3:21),UpperIdent(3:22-3:25),
LowerIdent(4:1-4:12),OpAssign(4:13-4:14),OpBar(4:15-4:16),LowerIdent(4:16-4:20),OpBar(4:20-4:21),
StringStart(5:5-5:6),StringPart(5:6-5:15),StringEnd(5:15-5:16),
UpperIdent(8:1-8:12),OpColon(8:13-8:14),OpenCurly(8:15-8:16),
UpperIdent(9:5-9:11),OpColon(9:12-9:13),OpenSquare(9:14-9:15),UpperIdent(9:15-9:22),Comma(9:22-9:23),UpperIdent(9:24-9:31),CloseSquare(9:31-9:32),
CloseCurly(10:1-10:2),
EndOfFile(11:1-11:1),
~~~
# PARSE
~~~clojure
(file @1.1-10.2
	(type-module @1.1-1.7)
	(statements
		(s-type-decl @1.1-1.31
			(header @1.1-1.13 (name "Result")
				(args
					(ty-var @1.8-1.9 (raw "a"))
					(ty-var @1.11-1.12 (raw "b"))))
			(ty-tag-union @1.16-1.31
				(tags
					(ty-apply @1.17-1.22
						(ty @1.17-1.19 (name "Ok"))
						(ty-var @1.20-1.21 (raw "a")))
					(ty-apply @1.24-1.30
						(ty @1.24-1.27 (name "Err"))
						(ty-var @1.28-1.29 (raw "b"))))))
		(s-type-anno @3.1-3.25 (name "processData")
			(ty-fn @3.15-3.25
				(ty @3.15-3.18 (name "Str"))
				(ty @3.22-3.25 (name "Str"))))
		(s-decl @4.1-5.16
			(p-ident @4.1-4.12 (raw "processData"))
			(e-lambda @4.15-5.16
				(args
					(p-ident @4.16-4.20 (raw "data")))
				(e-string @5.5-5.16
					(e-string-part @5.6-5.15 (raw "processed")))))
		(s-type-decl @8.1-9.31
			(header @8.1-8.12 (name "InnerModule")
				(args))
			(ty-malformed @9.24-9.31 (tag "expected_ty_close_curly_or_comma")))
		(s-malformed @9.31-9.32 (tag "statement_unexpected_token"))
		(s-malformed @10.1-10.2 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
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
	(d-let
		(p-assign @4.1-4.12 (ident "processData"))
		(e-lambda @4.15-5.16
			(args
				(p-assign @4.16-4.20 (ident "data")))
			(e-string @5.5-5.16
				(e-literal @5.6-5.15 (string "processed"))))
		(annotation @4.1-4.12
			(declared-type
				(ty-fn @3.15-3.25 (effectful false)
					(ty @3.15-3.18 (name "Str"))
					(ty @3.22-3.25 (name "Str"))))))
	(s-alias-decl @1.1-1.31
		(ty-header @1.1-1.13 (name "Result")
			(ty-args
				(ty-var @1.8-1.9 (name "a"))
				(ty-var @1.11-1.12 (name "b"))))
		(ty-tag-union @1.16-1.31
			(ty-apply @1.17-1.22 (symbol "Ok")
				(ty-var @1.20-1.21 (name "a")))
			(ty-apply @1.24-1.30 (symbol "Err")
				(ty-var @1.28-1.29 (name "b")))))
	(s-alias-decl @8.1-9.31
		(ty-header @8.1-8.12 (name "InnerModule"))
		(ty-malformed @9.24-9.31)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.12 (type "Str -> Str")))
	(type_decls
		(alias @1.1-1.31 (type "Result(a, b)")
			(ty-header @1.1-1.13 (name "Result")
				(ty-args
					(ty-var @1.8-1.9 (name "a"))
					(ty-var @1.11-1.12 (name "b")))))
		(alias @8.1-9.31 (type "Error")
			(ty-header @8.1-8.12 (name "InnerModule"))))
	(expressions
		(expr @4.15-5.16 (type "Str -> Str"))))
~~~
