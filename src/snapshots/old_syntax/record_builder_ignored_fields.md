# META
~~~ini
description=record_builder_ignored_fields
type=expr
~~~
# SOURCE
~~~roc
{ Foo.Bar.baz <- x: 5, y: 0, _z: 3, _: 2
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - record_builder_ignored_fields.md:1:15:1:19
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_builder_ignored_fields.md:1:21:1:23
UNEXPECTED TOKEN IN EXPRESSION - record_builder_ignored_fields.md:1:22:1:25
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_builder_ignored_fields.md:1:27:1:29
UNEXPECTED TOKEN IN EXPRESSION - record_builder_ignored_fields.md:1:28:1:32
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_builder_ignored_fields.md:1:34:1:36
UNEXPECTED TOKEN IN EXPRESSION - record_builder_ignored_fields.md:1:35:1:38
UNEXPECTED TOKEN IN EXPRESSION - record_builder_ignored_fields.md:1:37:1:39
UNEXPECTED TOKEN IN EXPRESSION - record_builder_ignored_fields.md:1:38:1:41
UNDEFINED VARIABLE - record_builder_ignored_fields.md:1:3:1:14
MALFORMED TYPE - record_builder_ignored_fields.md:1:21:1:23
MALFORMED TYPE - record_builder_ignored_fields.md:1:27:1:29
MALFORMED TYPE - record_builder_ignored_fields.md:1:34:1:36
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **<- x** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_builder_ignored_fields.md:1:15:1:19:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0, _z: 3, _: 2
```
              ^^^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **5,** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_builder_ignored_fields.md:1:21:1:23:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0, _z: 3, _: 2
```
                    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, y** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_builder_ignored_fields.md:1:22:1:25:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0, _z: 3, _: 2
```
                     ^^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **0,** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_builder_ignored_fields.md:1:27:1:29:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0, _z: 3, _: 2
```
                          ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, _z** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_builder_ignored_fields.md:1:28:1:32:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0, _z: 3, _: 2
```
                           ^^^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **3,** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_builder_ignored_fields.md:1:34:1:36:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0, _z: 3, _: 2
```
                                 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, _** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_builder_ignored_fields.md:1:35:1:38:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0, _z: 3, _: 2
```
                                  ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **_:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_builder_ignored_fields.md:1:37:1:39:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0, _z: 3, _: 2
```
                                    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: 2** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_builder_ignored_fields.md:1:38:1:41:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0, _z: 3, _: 2
```
                                     ^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),UpperIdent(1:3-1:6),NoSpaceDotUpperIdent(1:6-1:10),NoSpaceDotLowerIdent(1:10-1:14),OpBackArrow(1:15-1:17),LowerIdent(1:18-1:19),OpColon(1:19-1:20),Int(1:21-1:22),Comma(1:22-1:23),LowerIdent(1:24-1:25),OpColon(1:25-1:26),Int(1:27-1:28),Comma(1:28-1:29),NamedUnderscore(1:30-1:32),OpColon(1:32-1:33),Int(1:34-1:35),Comma(1:35-1:36),Underscore(1:37-1:38),OpColon(1:38-1:39),Int(1:40-1:41),Newline(1:1-1:1),
CloseCurly(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-2.2
	(statements
		(e-ident @1.3-1.14 (raw "Foo.Bar.baz"))
		(e-malformed @1.15-1.19 (reason "expr_unexpected_token"))
		(s-type-anno @1.18-1.23 (name "x")
			(ty-malformed @1.21-1.23 (tag "ty_anno_unexpected_token")))
		(e-malformed @1.22-1.25 (reason "expr_unexpected_token"))
		(s-type-anno @1.24-1.29 (name "y")
			(ty-malformed @1.27-1.29 (tag "ty_anno_unexpected_token")))
		(e-malformed @1.28-1.32 (reason "expr_unexpected_token"))
		(s-type-anno @1.30-1.36 (name "_z")
			(ty-malformed @1.34-1.36 (tag "ty_anno_unexpected_token")))
		(e-malformed @1.35-1.38 (reason "expr_unexpected_token"))
		(e-malformed @1.37-1.39 (reason "expr_unexpected_token"))
		(e-malformed @1.38-1.41 (reason "expr_unexpected_token"))
		(e-int @1.40-1.41 (raw "2"))))
~~~
# FORMATTED
~~~roc
{
	Foo.baz
	
	x : 
	
	y : 
	
	_z : 
	
	
	
	2
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-2.2
	(s-expr @1.3-1.17
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-type-anno @1.18-1.23 (name "x")
		(ty-malformed @1.21-1.23))
	(s-type-anno @1.24-1.29 (name "y")
		(ty-malformed @1.27-1.29))
	(s-type-anno @1.30-1.36 (name "_z")
		(ty-malformed @1.34-1.36))
	(e-int @1.40-1.41 (value "2")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "Num(*)"))
~~~
