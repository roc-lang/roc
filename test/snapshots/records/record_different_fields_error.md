# META
~~~ini
description=Record with special character fields (error cases)
type=expr
~~~
# SOURCE
~~~roc
{
    _privateField: "leading underscore",
    field_: "trailing underscore",
    PascalCase: "pascal",
    kebab-case: "kebab",
    field$special: "dollar",
    field@symbol: "at symbol",
}
~~~
# EXPECTED
STRAY DOLLAR SIGN - record_different_fields_error.md:6:10:6:11
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_different_fields_error.md:2:20:2:21
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:2:21:2:39
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:2:39:2:40
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:2:40:2:41
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_different_fields_error.md:3:13:3:14
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:3:14:3:33
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:3:33:3:34
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:3:34:3:35
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:4:15:4:16
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:4:25:4:26
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:5:15:5:16
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:5:24:5:25
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_different_fields_error.md:6:20:6:21
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:21:6:27
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:27:6:28
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:28:6:29
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:7:10:7:17
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:7:17:7:18
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:7:30:7:31
DECLARATION HAS NO VALUE - record_different_fields_error.md:2:5:2:21
DECLARATION HAS NO VALUE - record_different_fields_error.md:3:5:3:14
TYPE MISMATCH - record_different_fields_error.md:4:5:4:15
MISSING METHOD - record_different_fields_error.md:4:17:4:25
MISSING METHOD - record_different_fields_error.md:5:17:5:24
DECLARATION HAS NO VALUE - record_different_fields_error.md:6:5:6:21
MISSING METHOD - record_different_fields_error.md:7:19:7:30
# PROBLEMS

┌───────────────────┐
│ STRAY DOLLAR SIGN ├─ Dollar sign ($) is only allowed at the very ───────────┐
└┬──────────────────┘  beginning of a name, not in the middle or at the end.  │
 │                                                                            │
 │  field$special: "dollar",                                                  │
 │       ‾                                                                    │
 └───────────────────────────────────── record_different_fields_error.md:6:10 ┘



┌─────────────────────────────────────┐
│ UNEXPECTED TOKEN IN TYPE ANNOTATION ├─ The token " is not expected in a ────┐
└┬────────────────────────────────────┘  type annotation.                     │
 │                                                                            │
 │  _privateField: "leading underscore",                                      │
 │                 ‾                                                          │
 └───────────────────────────────────── record_different_fields_error.md:2:20 ┘

    Type annotations should contain types like Str, Num a, or List U64.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token leading underscore is not ──────┐
└┬───────────────────────────────┘  expected in an expression.                │
 │                                                                            │
 │  _privateField: "leading underscore",                                      │
 │                  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                        │
 └───────────────────────────────────── record_different_fields_error.md:2:21 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token " is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  _privateField: "leading underscore",                                      │
 │                                    ‾                                       │
 └───────────────────────────────────── record_different_fields_error.md:2:39 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  _privateField: "leading underscore",                                      │
 │                                     ‾                                      │
 └───────────────────────────────────── record_different_fields_error.md:2:40 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌─────────────────────────────────────┐
│ UNEXPECTED TOKEN IN TYPE ANNOTATION ├─ The token " is not expected in a ────┐
└┬────────────────────────────────────┘  type annotation.                     │
 │                                                                            │
 │  field_: "trailing underscore",                                            │
 │          ‾                                                                 │
 └───────────────────────────────────── record_different_fields_error.md:3:13 ┘

    Type annotations should contain types like Str, Num a, or List U64.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token trailing underscore is not ─────┐
└┬───────────────────────────────┘  expected in an expression.                │
 │                                                                            │
 │  field_: "trailing underscore",                                            │
 │           ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                              │
 └───────────────────────────────────── record_different_fields_error.md:3:14 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token " is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  field_: "trailing underscore",                                            │
 │                              ‾                                             │
 └───────────────────────────────────── record_different_fields_error.md:3:33 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  field_: "trailing underscore",                                            │
 │                               ‾                                            │
 └───────────────────────────────────── record_different_fields_error.md:3:34 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token : is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  PascalCase: "pascal",                                                     │
 │            ‾                                                               │
 └───────────────────────────────────── record_different_fields_error.md:4:15 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  PascalCase: "pascal",                                                     │
 │                      ‾                                                     │
 └───────────────────────────────────── record_different_fields_error.md:4:25 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token : is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  kebab-case: "kebab",                                                      │
 │            ‾                                                               │
 └───────────────────────────────────── record_different_fields_error.md:5:15 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  kebab-case: "kebab",                                                      │
 │                     ‾                                                      │
 └───────────────────────────────────── record_different_fields_error.md:5:24 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌─────────────────────────────────────┐
│ UNEXPECTED TOKEN IN TYPE ANNOTATION ├─ The token " is not expected in a ────┐
└┬────────────────────────────────────┘  type annotation.                     │
 │                                                                            │
 │  field$special: "dollar",                                                  │
 │                 ‾                                                          │
 └───────────────────────────────────── record_different_fields_error.md:6:20 ┘

    Type annotations should contain types like Str, Num a, or List U64.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token dollar is not expected in an ───┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  field$special: "dollar",                                                  │
 │                  ‾‾‾‾‾‾                                                    │
 └───────────────────────────────────── record_different_fields_error.md:6:21 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token " is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  field$special: "dollar",                                                  │
 │                        ‾                                                   │
 └───────────────────────────────────── record_different_fields_error.md:6:27 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  field$special: "dollar",                                                  │
 │                         ‾                                                  │
 └───────────────────────────────────── record_different_fields_error.md:6:28 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token @symbol is not expected in an ──┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  field@symbol: "at symbol",                                                │
 │       ‾‾‾‾‾‾‾                                                              │
 └───────────────────────────────────── record_different_fields_error.md:7:10 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token : is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  field@symbol: "at symbol",                                                │
 │              ‾                                                             │
 └───────────────────────────────────── record_different_fields_error.md:7:17 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  field@symbol: "at symbol",                                                │
 │                           ‾                                                │
 └───────────────────────────────────── record_different_fields_error.md:7:30 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  _privateField: "leading underscore",                                      │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                          │
 └────────────────────────────────────── record_different_fields_error.md:2:5 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  field_: "trailing underscore",                                            │
 │  ‾‾‾‾‾‾‾‾‾                                                                 │
 └────────────────────────────────────── record_different_fields_error.md:3:5 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌───────────────┐
│ TYPE MISMATCH ├─ This expression produces a value, but it's not being ──────┐
└┬──────────────┘  used.                                                      │
 │                                                                            │
 │  PascalCase: "pascal",                                                     │
 │  ‾‾‾‾‾‾‾‾‾‾                                                                │
 └────────────────────────────────────── record_different_fields_error.md:4:5 ┘

    It has the type:

        [PascalCase, ..]

    Since this expression is used as a statement, it must evaluate to `{}`.
    If you don't need the value, you can ignore it with `_ =`.


┌────────────────┐
│ MISSING METHOD ├─ This `from_quote` method is being called on a value ──────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  PascalCase: "pascal",                                                     │
 │              ‾‾‾‾‾‾‾‾                                                      │
 └───────────────────────────────────── record_different_fields_error.md:4:17 ┘

    The value's type, which does not have a method named `from_quote`, is:

        {}


┌────────────────┐
│ MISSING METHOD ├─ This `from_quote` method is being called on a value ──────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  kebab-case: "kebab",                                                      │
 │              ‾‾‾‾‾‾‾                                                       │
 └───────────────────────────────────── record_different_fields_error.md:5:17 ┘

    The value's type, which does not have a method named `from_quote`, is:

        {}


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  field$special: "dollar",                                                  │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                          │
 └────────────────────────────────────── record_different_fields_error.md:6:5 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌────────────────┐
│ MISSING METHOD ├─ This `from_quote` method is being called on a value ──────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  field@symbol: "at symbol",                                                │
 │                ‾‾‾‾‾‾‾‾‾‾‾                                                 │
 └───────────────────────────────────── record_different_fields_error.md:7:19 ┘

    The value's type, which does not have a method named `from_quote`, is:

        {}

# TOKENS
~~~zig
OpenCurly,
NamedUnderscore,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
UpperIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpUnaryMinus,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpaqueName,OpColon,StringStart,StringPart,StringEnd,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-type-anno (name "_privateField")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(s-type-anno (name "field_")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-tag (raw "PascalCase"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-string
			(e-string-part (raw "pascal")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-ident (raw "kebab"))
		(unary "-"
			(e-ident (raw "case")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-string
			(e-string-part (raw "kebab")))
		(e-malformed (reason "expr_unexpected_token"))
		(s-type-anno (name "field$special")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-ident (raw "field"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-string
			(e-string-part (raw "at symbol")))
		(e-malformed (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	_privateField : 
			
	field_ : 
			
	PascalCase
		"pascal"
	
	kebab
	-case
		"kebab"
	
	field$special : 
			
	field
			"at symbol"
	
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "_privateField"))
		(e-anno-only))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "field_"))
		(e-anno-only))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-tag (name "PascalCase")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-string
			(e-literal (string "pascal"))))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expr
		(e-dispatch-call (method "negate") (constraint-fn-var 131)
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))
			(args)))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-string
			(e-literal (string "kebab"))))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "field$special"))
		(e-anno-only))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-string
			(e-literal (string "at symbol"))))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
