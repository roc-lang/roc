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
OpenCurly(1:1-1:2),LowerIdent(1:3-1:9),OpAmpersand(1:10-1:11),LowerIdent(1:12-1:15),OpColon(1:15-1:16),Int(1:17-1:19),CloseCurly(1:20-1:21),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-block @1.1-1.21
	(statements
		(e-ident @1.3-1.9 (raw "person"))
		(e-malformed @1.10-1.11 (reason "expr_unexpected_token"))
		(s-type-anno @1.12-1.19 (name "age")
			(ty-malformed @1.17-1.19 (tag "ty_anno_unexpected_token")))))
~~~
# FORMATTED
~~~roc
MALFORMED INPUT
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-1.21
	(s-expr @1.3-1.9
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expr @1.10-1.11
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-type-anno @1.12-1.19 (name "age")
		(ty-malformed @1.17-1.19))
	(e-empty_record @1.1-1.21))
~~~
# TYPES
~~~clojure
(expr @1.1-1.21 (type "{}"))
~~~
