# META
~~~ini
description=Record extension adding new fields
type=expr
~~~
# SOURCE
~~~roc
{ person & age: 31, active: Bool.true }
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **& age** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_extension_update.md:1:10:1:15:**
```roc
{ person & age: 31, active: Bool.true }
```


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **31,** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_extension_update.md:1:17:1:20:**
```roc
{ person & age: 31, active: Bool.true }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, active** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_extension_update.md:1:19:1:27:**
```roc
{ person & age: 31, active: Bool.true }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.true }** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_extension_update.md:1:33:1:40:**
```roc
{ person & age: 31, active: Bool.true }
```


**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:9),OpAmpersand(1:10-1:11),LowerIdent(1:12-1:15),OpColon(1:15-1:16),Int(1:17-1:19),Comma(1:19-1:20),LowerIdent(1:21-1:27),OpColon(1:27-1:28),UpperIdent(1:29-1:33),NoSpaceDotLowerIdent(1:33-1:38),CloseCurly(1:39-1:40),EndOfFile(1:40-1:40),
~~~
# PARSE
~~~clojure
(e-block @1-1-1-40
	(statements
		(e-ident @1-3-1-9 (qaul "") (raw "person"))
		(e-malformed @1-10-1-15 (reason "expr_unexpected_token"))
		(s-type-anno @1-12-1-20 (name "age")
			(ty-malformed @1-17-1-20 (tag "ty_anno_unexpected_token")))
		(e-malformed @1-19-1-27 (reason "expr_unexpected_token"))
		(s-type-anno @1-21-1-38 (name "active")
			(ty (name "Bool")))
		(e-malformed @1-33-1-40 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	person
	
	age : 
	
	active : Bool
	
}
~~~
# TYPES
~~~clojure
(expr (id 83) (type "*"))
~~~