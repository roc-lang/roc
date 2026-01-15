# META
~~~ini
description=Test nominal type with record backing
type=file
~~~
# SOURCE
~~~roc
module [point]

Point := { x: I64, y: I64 }

point = @Point({ x: 0, y: 0 })
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **@Point** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_nominal_type_record_backing.md:5:9:5:15:**
```roc
point = @Point({ x: 0, y: 0 })
```
        ^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_nominal_type_record_backing.md:5:15:5:16:**
```roc
point = @Point({ x: 0, y: 0 })
```
              ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_nominal_type_record_backing.md:5:16:5:17:**
```roc
point = @Point({ x: 0, y: 0 })
```
               ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **0** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**can_nominal_type_record_backing.md:5:21:5:22:**
```roc
point = @Point({ x: 0, y: 0 })
```
                    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_nominal_type_record_backing.md:5:22:5:23:**
```roc
point = @Point({ x: 0, y: 0 })
```
                     ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **0** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**can_nominal_type_record_backing.md:5:27:5:28:**
```roc
point = @Point({ x: 0, y: 0 })
```
                          ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_nominal_type_record_backing.md:5:29:5:30:**
```roc
point = @Point({ x: 0, y: 0 })
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_nominal_type_record_backing.md:5:30:5:31:**
```roc
point = @Point({ x: 0, y: 0 })
```
                             ^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_nominal_type_record_backing.md:1:1:1:15:**
```roc
module [point]
```
^^^^^^^^^^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**can_nominal_type_record_backing.md:5:9:5:15:**
```roc
point = @Point({ x: 0, y: 0 })
```
        ^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**can_nominal_type_record_backing.md:5:21:5:22:**
```roc
point = @Point({ x: 0, y: 0 })
```
                    ^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**can_nominal_type_record_backing.md:5:27:5:28:**
```roc
point = @Point({ x: 0, y: 0 })
```
                          ^


# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpAssign,OpaqueName,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-lower-ident
				(text "point"))))
	(statements
		(s-type-decl
			(header (name "Point")
				(args))
			(ty-record
				(anno-record-field (name "x")
					(ty (name "I64")))
				(anno-record-field (name "y")
					(ty (name "I64")))))
		(s-decl
			(p-ident (raw "point"))
			(e-malformed (reason "expr_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "x")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "y")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [point]

Point := { x : I64, y : I64 }

point = 
x : 
y : 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "point"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(d-let
		(p-assign (ident "x"))
		(e-anno-only)
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "y"))
		(e-anno-only)
		(annotation
			(ty-malformed)))
	(s-nominal-decl
		(ty-header (name "Point"))
		(ty-record
			(field (field "x")
				(ty-lookup (name "I64") (builtin)))
			(field (field "y")
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "Point")
			(ty-header (name "Point"))))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
