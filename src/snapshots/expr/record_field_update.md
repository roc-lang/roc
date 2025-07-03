# META
~~~ini
description=Record with field update syntax
type=expr
~~~
# SOURCE
~~~roc
{ ..person, age: 31 }
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **..person** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_field_update.md:1:3:1:11:**
```roc
{ ..person, age: 31 }
```
  ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, age** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_field_update.md:1:11:1:16:**
```roc
{ ..person, age: 31 }
```
          ^^^^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **31 }** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_field_update.md:1:18:1:22:**
```roc
{ ..person, age: 31 }
```
                 ^^^^


**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

# TOKENS
~~~zig
OpenCurly(1:1-1:2),DoubleDot(1:3-1:5),LowerIdent(1:5-1:11),Comma(1:11-1:12),LowerIdent(1:13-1:16),OpColon(1:16-1:17),Int(1:18-1:20),CloseCurly(1:21-1:22),EndOfFile(1:22-1:22),
~~~
# PARSE
~~~clojure
(e-block @1.1-1.22
	(statements
		(e-malformed @1.3-1.11 (reason "expr_unexpected_token"))
		(e-ident @1.5-1.11 (qaul "") (raw "person"))
		(e-malformed @1.11-1.16 (reason "expr_unexpected_token"))
		(s-type-anno @1.13-1.22 (name "age")
			(ty-malformed @1.18-1.22 (tag "ty_anno_unexpected_token")))))
~~~
# FORMATTED
~~~roc
{
	
	person
	
	age : 
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-1.22
	(s-expr @1.5-1.12
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-type-anno @1.13-1.22 (name "age")
		(ty-malformed @1.18-1.22))
	(e-tuple @1.13-1.22
		(elems)))
~~~
# TYPES
~~~clojure
(expr @1.1-1.22 (type "()"))
~~~
