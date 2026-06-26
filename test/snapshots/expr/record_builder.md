# META
~~~ini
description=record_builder
type=expr
~~~
# SOURCE
~~~roc
{ Foo.Bar.baz <-
    x: 5,
    y: 0,
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - record_builder.md:1:15:1:17
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_builder.md:2:8:2:9
UNEXPECTED TOKEN IN EXPRESSION - record_builder.md:2:9:2:10
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_builder.md:3:8:3:9
UNEXPECTED TOKEN IN EXPRESSION - record_builder.md:3:9:3:10
DECLARATION HAS NO VALUE - record_builder.md:2:5:2:9
DECLARATION HAS NO VALUE - record_builder.md:3:5:3:9
# PROBLEMS

┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token <- is not expected in an ───────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  { Foo.Bar.baz <-                                                          │
 │                ‾‾                                                          │
 └──────────────────────────────────────────────────── record_builder.md:1:15 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌─────────────────────────────────────┐
│ UNEXPECTED TOKEN IN TYPE ANNOTATION ├─ The token 5 is not expected in a ────┐
└┬────────────────────────────────────┘  type annotation.                     │
 │                                                                            │
 │  x: 5,                                                                     │
 │     ‾                                                                      │
 └───────────────────────────────────────────────────── record_builder.md:2:8 ┘

    Type annotations should contain types like Str, Num a, or List U64.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  x: 5,                                                                     │
 │      ‾                                                                     │
 └───────────────────────────────────────────────────── record_builder.md:2:9 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌─────────────────────────────────────┐
│ UNEXPECTED TOKEN IN TYPE ANNOTATION ├─ The token 0 is not expected in a ────┐
└┬────────────────────────────────────┘  type annotation.                     │
 │                                                                            │
 │  y: 0,                                                                     │
 │     ‾                                                                      │
 └───────────────────────────────────────────────────── record_builder.md:3:8 ┘

    Type annotations should contain types like Str, Num a, or List U64.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  y: 0,                                                                     │
 │      ‾                                                                     │
 └───────────────────────────────────────────────────── record_builder.md:3:9 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  x: 5,                                                                     │
 │  ‾‾‾‾                                                                      │
 └───────────────────────────────────────────────────── record_builder.md:2:5 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  y: 0,                                                                     │
 │  ‾‾‾‾                                                                      │
 └───────────────────────────────────────────────────── record_builder.md:3:5 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

# TOKENS
~~~zig
OpenCurly,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,OpBackArrow,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(e-ident (raw "Foo.Bar.baz"))
		(e-malformed (reason "expr_unexpected_token"))
		(s-type-anno (name "x")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))
		(s-type-anno (name "y")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	Foo.Bar.baz
	
	x : 
	
	y : 
	
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-expr
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "x"))
		(e-anno-only))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "y"))
		(e-anno-only))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
