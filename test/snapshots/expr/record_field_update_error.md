# META
~~~ini
description=Record with field update using old syntax (should give nice error message)
type=expr
~~~
# SOURCE
~~~roc
{ person & age: 31 }
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - record_field_update_error.md:1:10:1:11
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_field_update_error.md:1:17:1:19
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **&** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_field_update_error.md:1:10:1:11:**
```roc
{ person & age: 31 }
```
         ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **31** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**record_field_update_error.md:1:17:1:19:**
```roc
{ person & age: 31 }
```
                ^^


**DECLARATION HAS NO VALUE**
This declaration has a type annotation but no implementation.
**record_field_update_error.md:1:12:1:19:**
```roc
{ person & age: 31 }
```
           ^^^^^^^


Add a value body here, or put hosted functions in a platform type module so they are published through the host boundary.

# TOKENS
~~~zig
OpenCurly,LowerIdent,OpAmpersand,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(e-ident (raw "person"))
		(e-malformed (reason "expr_unexpected_token"))
		(s-type-anno (name "age")
			(ty-malformed (tag "ty_anno_unexpected_token")))))
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
(e-block
	(s-expr
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "age"))
		(e-anno-only))
	(e-empty_record))
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
