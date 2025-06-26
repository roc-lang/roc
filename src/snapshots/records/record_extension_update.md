# META
~~~ini
description=Record update syntax
type=expr
~~~
# SOURCE
~~~roc
{ person &
    age: 31,
    active: Bool.true,
}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_extension_update.md:1:10:1:10:**
```roc
{ person &
```


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **31,** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_extension_update.md:2:10:2:13:**
```roc
    age: 31,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_extension_update.md:2:12:2:12:**
```roc
    age: 31,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.true,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_extension_update.md:3:17:3:23:**
```roc
    active: Bool.true,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_extension_update.md:3:22:3:22:**
```roc
    active: Bool.true,
```


**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:9),OpAmpersand(1:10-1:11),Newline(1:1-1:1),
LowerIdent(2:5-2:8),OpColon(2:8-2:9),Int(2:10-2:12),Comma(2:12-2:13),Newline(1:1-1:1),
LowerIdent(3:5-3:11),OpColon(3:11-3:12),UpperIdent(3:13-3:17),NoSpaceDotLowerIdent(3:17-3:22),Comma(3:22-3:23),Newline(1:1-1:1),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-block @1-1-4-2
	(statements
		(e-ident @1-3-1-9 (qaul "") (raw "person"))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(s-type-anno @2-5-2-13 (name "age")
			(ty-malformed @2-10-2-13 (tag "ty_anno_unexpected_token")))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(s-type-anno @3-5-3-22 (name "active")
			(ty (name "Bool")))
		(e-malformed @3-17-3-23 (reason "expr_unexpected_token"))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))))
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