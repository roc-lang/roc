# META
~~~ini
description=Type shadowing across scopes should produce warning
type=snippet
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
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
StringStart,StringPart,StringEnd,
UpperIdent,OpColon,OpenCurly,
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Result")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Ok"))
						(ty-var (raw "a")))
					(ty-apply
						(ty (name "Err"))
						(ty-var (raw "b"))))))
		(s-type-anno (name "processData")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "processData"))
			(e-lambda
				(args
					(p-ident (raw "data")))
				(e-string
					(e-string-part (raw "processed")))))
		(s-type-decl
			(header (name "InnerModule")
				(args))
			(ty-malformed (tag "expected_ty_close_curly_or_comma")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
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
		(p-assign (ident "processData"))
		(e-lambda
			(args
				(p-assign (ident "data")))
			(e-string
				(e-literal (string "processed"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(s-alias-decl
		(ty-header (name "Result")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-tag-union
			(ty-tag-name (name "Ok")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-tag-name (name "Err")
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(s-alias-decl
		(ty-header (name "InnerModule"))
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Str")))
	(type_decls
		(alias (type "Result(a, b)")
			(ty-header (name "Result")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "InnerModule")
			(ty-header (name "InnerModule"))))
	(expressions
		(expr (type "Str -> Str"))))
~~~
