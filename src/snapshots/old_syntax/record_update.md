# META
~~~ini
description=record_update
type=expr
~~~
# SOURCE
~~~roc
{ Foo.Bar.baz & x: 5, y: 0 }
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Bar.baz** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_update.md:1:6:1:14:**
```roc
{ Foo.Bar.baz & x: 5, y: 0 }
```
     ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.baz &** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_update.md:1:10:1:16:**
```roc
{ Foo.Bar.baz & x: 5, y: 0 }
```
         ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **& x** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_update.md:1:15:1:18:**
```roc
{ Foo.Bar.baz & x: 5, y: 0 }
```
              ^^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **5,** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_update.md:1:20:1:22:**
```roc
{ Foo.Bar.baz & x: 5, y: 0 }
```
                   ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, y** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_update.md:1:21:1:24:**
```roc
{ Foo.Bar.baz & x: 5, y: 0 }
```
                    ^^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **0 }** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_update.md:1:26:1:29:**
```roc
{ Foo.Bar.baz & x: 5, y: 0 }
```
                         ^^^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

# TOKENS
~~~zig
OpenCurly(1:1-1:2),UpperIdent(1:3-1:6),NoSpaceDotUpperIdent(1:6-1:10),NoSpaceDotLowerIdent(1:10-1:14),OpAmpersand(1:15-1:16),LowerIdent(1:17-1:18),OpColon(1:18-1:19),Int(1:20-1:21),Comma(1:21-1:22),LowerIdent(1:23-1:24),OpColon(1:24-1:25),Int(1:26-1:27),CloseCurly(1:28-1:29),EndOfFile(1:29-1:29),
~~~
# PARSE
~~~clojure
(e-block @1.1-1.29
	(statements
		(e-tag @1.3-1.6 (raw "Foo"))
		(e-malformed @1.6-1.14 (reason "expr_unexpected_token"))
		(e-malformed @1.10-1.16 (reason "expr_unexpected_token"))
		(e-malformed @1.15-1.18 (reason "expr_unexpected_token"))
		(s-type-anno @1.17-1.22 (name "x")
			(ty-malformed @1.20-1.22 (tag "ty_anno_unexpected_token")))
		(e-malformed @1.21-1.24 (reason "expr_unexpected_token"))
		(s-type-anno @1.23-1.29 (name "y")
			(ty-malformed @1.26-1.29 (tag "ty_anno_unexpected_token")))))
~~~
# FORMATTED
~~~roc
{
	Foo
	
	
	
	x : 
	
	y : 
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-1.29
	(s-expr @1.3-1.10
		(e-tag @1.3-1.6 (name "Foo") (args "TODO")))
	(s-type-anno @1.17-1.22 (name "x")
		(ty-malformed @1.20-1.22))
	(s-type-anno @1.23-1.29 (name "y")
		(ty-malformed @1.26-1.29))
	(e-tuple @1.23-1.29
		(elems)))
~~~
# TYPES
~~~clojure
(expr @1.1-1.29 (type "*"))
~~~
