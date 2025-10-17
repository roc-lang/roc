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
UNDEFINED VARIABLE - record_field_update_error.md:1:3:1:9
UNRECOGNIZED SYNTAX - record_field_update_error.md:1:10:1:11
MALFORMED TYPE - record_field_update_error.md:1:17:1:19
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


**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_field_update_error.md:1:3:1:9:**
```roc
{ person & age: 31 }
```
  ^^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_field_update_error.md:1:10:1:11:**
```roc
{ person & age: 31 }
```
         ^

This might be a syntax error, an unsupported language feature, or a typo.

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**record_field_update_error.md:1:17:1:19:**
```roc
{ person & age: 31 }
```
                ^^


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
	(e-empty_record))
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
