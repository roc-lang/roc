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
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:10:6:11
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_different_fields_error.md:6:20:6:21
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:21:6:27
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:27:6:28
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:6:28:6:29
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:7:10:7:17
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:7:17:7:18
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_error.md:7:30:7:31
MALFORMED TYPE - record_different_fields_error.md:2:20:2:21
MALFORMED TYPE - record_different_fields_error.md:3:13:3:14
UNDEFINED VARIABLE - record_different_fields_error.md:5:5:5:10
UNDEFINED VARIABLE - record_different_fields_error.md:5:11:5:15
UNDEFINED VARIABLE - record_different_fields_error.md:6:5:6:10
MALFORMED TYPE - record_different_fields_error.md:6:20:6:21
UNDEFINED VARIABLE - record_different_fields_error.md:7:5:7:10
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_different_fields_error.md:2:20:2:21:**
```roc
    _privateField: "leading underscore",
```
                   ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **leading underscore** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:2:21:2:39:**
```roc
    _privateField: "leading underscore",
```
                    ^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:2:39:2:40:**
```roc
    _privateField: "leading underscore",
```
                                      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:2:40:2:41:**
```roc
    _privateField: "leading underscore",
```
                                       ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_different_fields_error.md:3:13:3:14:**
```roc
    field_: "trailing underscore",
```
            ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **trailing underscore** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:3:14:3:33:**
```roc
    field_: "trailing underscore",
```
             ^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:3:33:3:34:**
```roc
    field_: "trailing underscore",
```
                                ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:3:34:3:35:**
```roc
    field_: "trailing underscore",
```
                                 ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:4:15:4:16:**
```roc
    PascalCase: "pascal",
```
              ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:4:25:4:26:**
```roc
    PascalCase: "pascal",
```
                        ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:5:15:5:16:**
```roc
    kebab-case: "kebab",
```
              ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:5:24:5:25:**
```roc
    kebab-case: "kebab",
```
                       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **$** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:6:10:6:11:**
```roc
    field$special: "dollar",
```
         ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_different_fields_error.md:6:20:6:21:**
```roc
    field$special: "dollar",
```
                   ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dollar** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:6:21:6:27:**
```roc
    field$special: "dollar",
```
                    ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:6:27:6:28:**
```roc
    field$special: "dollar",
```
                          ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:6:28:6:29:**
```roc
    field$special: "dollar",
```
                           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **@symbol** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:7:10:7:17:**
```roc
    field@symbol: "at symbol",
```
         ^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:7:17:7:18:**
```roc
    field@symbol: "at symbol",
```
                ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:7:30:7:31:**
```roc
    field@symbol: "at symbol",
```
                             ^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**record_different_fields_error.md:2:20:2:21:**
```roc
    _privateField: "leading underscore",
```
                   ^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**record_different_fields_error.md:3:13:3:14:**
```roc
    field_: "trailing underscore",
```
            ^


**UNDEFINED VARIABLE**
Nothing is named `kebab` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_different_fields_error.md:5:5:5:10:**
```roc
    kebab-case: "kebab",
```
    ^^^^^


**UNDEFINED VARIABLE**
Nothing is named `case` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_different_fields_error.md:5:11:5:15:**
```roc
    kebab-case: "kebab",
```
          ^^^^


**UNDEFINED VARIABLE**
Nothing is named `field` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_different_fields_error.md:6:5:6:10:**
```roc
    field$special: "dollar",
```
    ^^^^^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**record_different_fields_error.md:6:20:6:21:**
```roc
    field$special: "dollar",
```
                   ^


**UNDEFINED VARIABLE**
Nothing is named `field` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_different_fields_error.md:7:5:7:10:**
```roc
    field@symbol: "at symbol",
```
    ^^^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),
NamedUnderscore(2:5-2:18),OpColon(2:18-2:19),StringStart(2:20-2:21),StringPart(2:21-2:39),StringEnd(2:39-2:40),Comma(2:40-2:41),
LowerIdent(3:5-3:11),OpColon(3:11-3:12),StringStart(3:13-3:14),StringPart(3:14-3:33),StringEnd(3:33-3:34),Comma(3:34-3:35),
UpperIdent(4:5-4:15),OpColon(4:15-4:16),StringStart(4:17-4:18),StringPart(4:18-4:24),StringEnd(4:24-4:25),Comma(4:25-4:26),
LowerIdent(5:5-5:10),OpUnaryMinus(5:10-5:11),LowerIdent(5:11-5:15),OpColon(5:15-5:16),StringStart(5:17-5:18),StringPart(5:18-5:23),StringEnd(5:23-5:24),Comma(5:24-5:25),
LowerIdent(6:5-6:10),MalformedUnknownToken(6:10-6:11),LowerIdent(6:11-6:18),OpColon(6:18-6:19),StringStart(6:20-6:21),StringPart(6:21-6:27),StringEnd(6:27-6:28),Comma(6:28-6:29),
LowerIdent(7:5-7:10),OpaqueName(7:10-7:17),OpColon(7:17-7:18),StringStart(7:19-7:20),StringPart(7:20-7:29),StringEnd(7:29-7:30),Comma(7:30-7:31),
CloseCurly(8:1-8:2),EndOfFile(8:2-8:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-8.2
	(statements
		(s-type-anno @2.5-2.21 (name "_privateField")
			(ty-malformed @2.20-2.21 (tag "ty_anno_unexpected_token")))
		(e-malformed @2.21-2.39 (reason "expr_unexpected_token"))
		(e-malformed @2.39-2.40 (reason "expr_unexpected_token"))
		(e-malformed @2.40-2.41 (reason "expr_unexpected_token"))
		(s-type-anno @3.5-3.14 (name "field_")
			(ty-malformed @3.13-3.14 (tag "ty_anno_unexpected_token")))
		(e-malformed @3.14-3.33 (reason "expr_unexpected_token"))
		(e-malformed @3.33-3.34 (reason "expr_unexpected_token"))
		(e-malformed @3.34-3.35 (reason "expr_unexpected_token"))
		(e-tag @4.5-4.15 (raw "PascalCase"))
		(e-malformed @4.15-4.16 (reason "expr_unexpected_token"))
		(e-string @4.17-4.25
			(e-string-part @4.18-4.24 (raw "pascal")))
		(e-malformed @4.25-4.26 (reason "expr_unexpected_token"))
		(e-ident @5.5-5.10 (raw "kebab"))
		(unary "-"
			(e-ident @5.11-5.15 (raw "case")))
		(e-malformed @5.15-5.16 (reason "expr_unexpected_token"))
		(e-string @5.17-5.24
			(e-string-part @5.18-5.23 (raw "kebab")))
		(e-malformed @5.24-5.25 (reason "expr_unexpected_token"))
		(e-ident @6.5-6.10 (raw "field"))
		(e-malformed @6.10-6.11 (reason "expr_unexpected_token"))
		(s-type-anno @6.11-6.21 (name "special")
			(ty-malformed @6.20-6.21 (tag "ty_anno_unexpected_token")))
		(e-malformed @6.21-6.27 (reason "expr_unexpected_token"))
		(e-malformed @6.27-6.28 (reason "expr_unexpected_token"))
		(e-malformed @6.28-6.29 (reason "expr_unexpected_token"))
		(e-ident @7.5-7.10 (raw "field"))
		(e-malformed @7.10-7.17 (reason "expr_unexpected_token"))
		(e-malformed @7.17-7.18 (reason "expr_unexpected_token"))
		(e-string @7.19-7.30
			(e-string-part @7.20-7.29 (raw "at symbol")))
		(e-malformed @7.30-7.31 (reason "expr_unexpected_token"))))
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
	
	field
	
	special : 
	
	
	
	field
	
	
	"at symbol"
	
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-8.2
	(s-type-anno @2.5-2.21 (name "_privateField")
		(ty-malformed @2.20-2.21))
	(s-type-anno @3.5-3.14 (name "field_")
		(ty-malformed @3.13-3.14))
	(s-expr @4.5-4.15
		(e-tag @4.5-4.15 (name "PascalCase")))
	(s-expr @4.17-4.25
		(e-string @4.17-4.25
			(e-literal @4.18-4.24 (string "pascal"))))
	(s-expr @5.5-5.10
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expr @5.10-5.15
		(e-unary-minus @5.10-5.15
			(e-runtime-error (tag "ident_not_in_scope"))))
	(s-expr @5.17-5.24
		(e-string @5.17-5.24
			(e-literal @5.18-5.23 (string "kebab"))))
	(s-expr @6.5-6.10
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-type-anno @6.11-6.21 (name "special")
		(ty-malformed @6.20-6.21))
	(s-expr @7.5-7.10
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expr @7.19-7.30
		(e-string @7.19-7.30
			(e-literal @7.20-7.29 (string "at symbol"))))
	(e-empty_record @1.1-8.2))
~~~
# TYPES
~~~clojure
(expr @1.1-8.2 (type "{}"))
~~~
