# META
~~~ini
description=Test nominal type instantiation with tag backing
type=file
~~~
# SOURCE
~~~roc
module [x]

Format := [Format]

x = @Format(Format)
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - can_nominal_type_instance.md:5:5:5:12
PARSE ERROR - can_nominal_type_instance.md:5:12:5:13
PARSE ERROR - can_nominal_type_instance.md:5:19:5:20
MODULE HEADER DEPRECATED - can_nominal_type_instance.md:1:1:1:11
UNRECOGNIZED SYNTAX - can_nominal_type_instance.md:5:5:5:12
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **@Format** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_nominal_type_instance.md:5:5:5:12:**
```roc
x = @Format(Format)
```
    ^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_nominal_type_instance.md:5:12:5:13:**
```roc
x = @Format(Format)
```
           ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**can_nominal_type_instance.md:5:19:5:20:**
```roc
x = @Format(Format)
```
                  ^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_nominal_type_instance.md:1:1:1:11:**
```roc
module [x]
```
^^^^^^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**can_nominal_type_instance.md:5:5:5:12:**
```roc
x = @Format(Format)
```
    ^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpaqueName,NoSpaceOpenRound,UpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-lower-ident
				(text "x"))))
	(statements
		(s-type-decl
			(header (name "Format")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Format")))))
		(s-decl
			(p-ident (raw "x"))
			(e-malformed (reason "expr_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
module [x]

Format := [Format]

x = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-nominal-decl
		(ty-header (name "Format"))
		(ty-tag-union
			(ty-tag-name (name "Format")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(type_decls
		(nominal (type "Format")
			(ty-header (name "Format"))))
	(expressions
		(expr (type "Error"))))
~~~
