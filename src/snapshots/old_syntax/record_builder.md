# META
~~~ini
description=record_builder
type=expr
~~~
# SOURCE
~~~roc
{ Foo.Bar.baz <- x: 5, y: 0
}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **<- x** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_builder.md:1:15:1:19:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0
```
              ^^^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **5,** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_builder.md:1:21:1:23:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0
```
                    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, y** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_builder.md:1:22:1:25:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0
```
                     ^^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token  is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_builder.md:1:27:1:27:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0
```
                          


**UNEXPECTED TOKEN IN EXPRESSION**
The token **{ Foo.Bar.baz <- x: 5, y: 0
}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_builder.md:1:1:2:2:**
```roc
{ Foo.Bar.baz <- x: 5, y: 0
}
```


**UNDEFINED VARIABLE**
Nothing is named `baz` in this scope.
Is there an `import` or `exposing` missing up-top?

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

# TOKENS
~~~zig
OpenCurly(1:1-1:2),UpperIdent(1:3-1:6),NoSpaceDotUpperIdent(1:6-1:10),NoSpaceDotLowerIdent(1:10-1:14),OpBackArrow(1:15-1:17),LowerIdent(1:18-1:19),OpColon(1:19-1:20),Int(1:21-1:22),Comma(1:22-1:23),LowerIdent(1:24-1:25),OpColon(1:25-1:26),Int(1:27-1:28),Newline(1:1-1:1),
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
		(s-type-anno @1.1-1.1 (name "y")
			(ty-malformed @1.1-1.1 (tag "ty_anno_unexpected_token")))
		(e-malformed @1.1-2.2 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	Foo.baz
	
	x : 
	
	y : 
	
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-2.2
	(s-expr @1.3-1.17
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-type-anno @1.18-1.23 (name "x")
		(ty-malformed @1.21-1.23))
	(s-type-anno @1.1-1.1 (name "y")
		(ty-malformed @1.1-1.1))
	(e-empty_record @1.1-2.2))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "{}"))
~~~
