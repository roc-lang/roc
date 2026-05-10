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
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **<-** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_builder.md:1:15:1:17:**
```roc
{ Foo.Bar.baz <-
```
              ^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **5** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**record_builder.md:2:8:2:9:**
```roc
    x: 5,
```
       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_builder.md:2:9:2:10:**
```roc
    x: 5,
```
        ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **0** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**record_builder.md:3:8:3:9:**
```roc
    y: 0,
```
       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_builder.md:3:9:3:10:**
```roc
    y: 0,
```
        ^


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
