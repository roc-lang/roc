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
UNRECOGNIZED SYNTAX - record_different_fields_error.md:2:21:2:39
UNRECOGNIZED SYNTAX - record_different_fields_error.md:2:39:2:40
UNRECOGNIZED SYNTAX - record_different_fields_error.md:2:40:2:41
MALFORMED TYPE - record_different_fields_error.md:3:13:3:14
UNRECOGNIZED SYNTAX - record_different_fields_error.md:3:14:3:33
UNRECOGNIZED SYNTAX - record_different_fields_error.md:3:33:3:34
UNRECOGNIZED SYNTAX - record_different_fields_error.md:3:34:3:35
UNRECOGNIZED SYNTAX - record_different_fields_error.md:4:15:4:16
UNRECOGNIZED SYNTAX - record_different_fields_error.md:4:25:4:26
UNDEFINED VARIABLE - record_different_fields_error.md:5:5:5:10
UNDEFINED VARIABLE - record_different_fields_error.md:5:11:5:15
UNRECOGNIZED SYNTAX - record_different_fields_error.md:5:15:5:16
UNRECOGNIZED SYNTAX - record_different_fields_error.md:5:24:5:25
UNDEFINED VARIABLE - record_different_fields_error.md:6:5:6:10
UNRECOGNIZED SYNTAX - record_different_fields_error.md:6:10:6:11
MALFORMED TYPE - record_different_fields_error.md:6:20:6:21
UNRECOGNIZED SYNTAX - record_different_fields_error.md:6:21:6:27
UNRECOGNIZED SYNTAX - record_different_fields_error.md:6:27:6:28
UNRECOGNIZED SYNTAX - record_different_fields_error.md:6:28:6:29
UNDEFINED VARIABLE - record_different_fields_error.md:7:5:7:10
UNRECOGNIZED SYNTAX - record_different_fields_error.md:7:10:7:17
UNRECOGNIZED SYNTAX - record_different_fields_error.md:7:17:7:18
UNRECOGNIZED SYNTAX - record_different_fields_error.md:7:30:7:31
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**record_different_fields_error.md:2:20:2:21:**
```roc
    _privateField: "leading underscore",
```
                   ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **leading underscore** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:2:21:2:39:**
```roc
    _privateField: "leading underscore",
```
                    ^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:2:39:2:40:**
```roc
    _privateField: "leading underscore",
```
                                      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:2:40:2:41:**
```roc
    _privateField: "leading underscore",
```
                                       ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**record_different_fields_error.md:3:13:3:14:**
```roc
    field_: "trailing underscore",
```
            ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **trailing underscore** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:3:14:3:33:**
```roc
    field_: "trailing underscore",
```
             ^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:3:33:3:34:**
```roc
    field_: "trailing underscore",
```
                                ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:3:34:3:35:**
```roc
    field_: "trailing underscore",
```
                                 ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:4:15:4:16:**
```roc
    PascalCase: "pascal",
```
              ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:4:25:4:26:**
```roc
    PascalCase: "pascal",
```
                        ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:5:15:5:16:**
```roc
    kebab-case: "kebab",
```
              ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:5:24:5:25:**
```roc
    kebab-case: "kebab",
```
                       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **$** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:6:10:6:11:**
```roc
    field$special: "dollar",
```
         ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**record_different_fields_error.md:6:20:6:21:**
```roc
    field$special: "dollar",
```
                   ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dollar** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:6:21:6:27:**
```roc
    field$special: "dollar",
```
                    ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:6:27:6:28:**
```roc
    field$special: "dollar",
```
                          ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:6:28:6:29:**
```roc
    field$special: "dollar",
```
                           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **@symbol** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:7:10:7:17:**
```roc
    field@symbol: "at symbol",
```
         ^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_error.md:7:17:7:18:**
```roc
    field@symbol: "at symbol",
```
                ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

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


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:2:21:2:39:**
```roc
    _privateField: "leading underscore",
```
                    ^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:2:39:2:40:**
```roc
    _privateField: "leading underscore",
```
                                      ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:2:40:2:41:**
```roc
    _privateField: "leading underscore",
```
                                       ^

This might be a syntax error, an unsupported language feature, or a typo.

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**record_different_fields_error.md:3:13:3:14:**
```roc
    field_: "trailing underscore",
```
            ^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:3:14:3:33:**
```roc
    field_: "trailing underscore",
```
             ^^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:3:33:3:34:**
```roc
    field_: "trailing underscore",
```
                                ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:3:34:3:35:**
```roc
    field_: "trailing underscore",
```
                                 ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:4:15:4:16:**
```roc
    PascalCase: "pascal",
```
              ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:4:25:4:26:**
```roc
    PascalCase: "pascal",
```
                        ^

This might be a syntax error, an unsupported language feature, or a typo.

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


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:5:15:5:16:**
```roc
    kebab-case: "kebab",
```
              ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:5:24:5:25:**
```roc
    kebab-case: "kebab",
```
                       ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNDEFINED VARIABLE**
Nothing is named `field` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_different_fields_error.md:6:5:6:10:**
```roc
    field$special: "dollar",
```
    ^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:6:10:6:11:**
```roc
    field$special: "dollar",
```
         ^

This might be a syntax error, an unsupported language feature, or a typo.

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**record_different_fields_error.md:6:20:6:21:**
```roc
    field$special: "dollar",
```
                   ^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:6:21:6:27:**
```roc
    field$special: "dollar",
```
                    ^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:6:27:6:28:**
```roc
    field$special: "dollar",
```
                          ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:6:28:6:29:**
```roc
    field$special: "dollar",
```
                           ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNDEFINED VARIABLE**
Nothing is named `field` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_different_fields_error.md:7:5:7:10:**
```roc
    field@symbol: "at symbol",
```
    ^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:7:10:7:17:**
```roc
    field@symbol: "at symbol",
```
         ^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:7:17:7:18:**
```roc
    field@symbol: "at symbol",
```
                ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_error.md:7:30:7:31:**
```roc
    field@symbol: "at symbol",
```
                             ^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
OpenCurly,
NamedUnderscore,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
UpperIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpUnaryMinus,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,MalformedUnknownToken,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
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
		(e-ident (raw "field"))
		(e-malformed (reason "expr_unexpected_token"))
		(s-type-anno (name "special")
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
	
	field
		special : 
			
	field
			"at symbol"
	
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
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
		(e-unary-minus
			(e-runtime-error (tag "ident_not_in_scope"))))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-string
			(e-literal (string "kebab"))))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
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
