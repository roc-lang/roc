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
UNDEFINED VARIABLE - record_builder.md:1:3:1:14
UNRECOGNIZED SYNTAX - record_builder.md:1:15:1:17
MALFORMED TYPE - record_builder.md:2:8:2:9
UNRECOGNIZED SYNTAX - record_builder.md:2:9:2:10
MALFORMED TYPE - record_builder.md:3:8:3:9
UNRECOGNIZED SYNTAX - record_builder.md:3:9:3:10
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


**UNDEFINED VARIABLE**
Nothing is named `baz` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_builder.md:1:3:1:14:**
```roc
{ Foo.Bar.baz <-
```
  ^^^^^^^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_builder.md:1:15:1:17:**
```roc
{ Foo.Bar.baz <-
```
              ^^

This might be a syntax error, an unsupported language feature, or a typo.

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**record_builder.md:2:8:2:9:**
```roc
    x: 5,
```
       ^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_builder.md:2:9:2:10:**
```roc
    x: 5,
```
        ^

This might be a syntax error, an unsupported language feature, or a typo.

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**record_builder.md:3:8:3:9:**
```roc
    y: 0,
```
       ^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_builder.md:3:9:3:10:**
```roc
    y: 0,
```
        ^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
OpenCurly(1:1-1:2),UpperIdent(1:3-1:6),NoSpaceDotUpperIdent(1:6-1:10),NoSpaceDotLowerIdent(1:10-1:14),OpBackArrow(1:15-1:17),
LowerIdent(2:5-2:6),OpColon(2:6-2:7),Int(2:8-2:9),Comma(2:9-2:10),
LowerIdent(3:5-3:6),OpColon(3:6-3:7),Int(3:8-3:9),Comma(3:9-3:10),
CloseCurly(4:1-4:2),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(e-block @1.1-4.2
	(statements
		(e-ident @1.3-1.14 (raw "Foo.Bar.baz"))
		(e-malformed @1.15-1.17 (reason "expr_unexpected_token"))
		(s-type-anno @2.5-2.9 (name "x")
			(ty-malformed @2.8-2.9 (tag "ty_anno_unexpected_token")))
		(e-malformed @2.9-2.10 (reason "expr_unexpected_token"))
		(s-type-anno @3.5-3.9 (name "y")
			(ty-malformed @3.8-3.9 (tag "ty_anno_unexpected_token")))
		(e-malformed @3.9-3.10 (reason "expr_unexpected_token"))))
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
(e-block @1.1-4.2
	(s-expr @1.3-1.14
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expr @1.15-1.17
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-type-anno @2.5-2.9 (name "x")
		(ty-malformed @2.8-2.9))
	(s-expr @2.9-2.10
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-type-anno @3.5-3.9 (name "y")
		(ty-malformed @3.8-3.9))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "Error"))
~~~
