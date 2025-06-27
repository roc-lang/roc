# META
~~~ini
description=Record with field update
type=expr
~~~
# SOURCE
~~~roc
{ person & age: 31 }
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **& age** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_field_update.md:1:10:1:15:**
```roc
{ person & age: 31 }
```


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **31 }** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_field_update.md:1:17:1:21:**
```roc
{ person & age: 31 }
```


**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:9),OpAmpersand(1:10-1:11),LowerIdent(1:12-1:15),OpColon(1:15-1:16),Int(1:17-1:19),CloseCurly(1:20-1:21),EndOfFile(1:21-1:21),
~~~
# PARSE
~~~clojure
(e-block @1-1-1-21
	(statements
		(e-ident @1-3-1-9 (qaul "") (raw "person"))
		(e-malformed @1-10-1-15 (reason "expr_unexpected_token"))
		(s-type-anno @1-12-1-21 (name "age")
			(ty-malformed @1-17-1-21 (tag "ty_anno_unexpected_token")))))
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
(e-block @1-1-1-21 (id 79)
	(s-expr @1-3-1-11
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-type-anno @1-12-1-21 (name "age")
		(ty-malformed @1-17-1-21))
	(e-tuple @1-12-1-21
		(elems)))
~~~
# TYPES
~~~clojure
(expr (id 79) (type "*"))
~~~